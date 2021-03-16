module GenerateGrammar where

-- argument parsing
import System.Environment (getArgs)
import System.Console.GetOpt
import ArgvParse

-- operations on file paths
import System.FilePath.Posix
import System.Directory

-- other system stuff
import System.Exit

import Data.Functor
import Data.List
import Data.Char (isAlpha,toLower)
import qualified Data.Map as M
import Data.Maybe

-- gf
import GF hiding (isPrefixOf, main, Label, Cat)
import GF.Support
import PGF

-- gf-ud
import RTree
import UDConcepts
import UDAnnotations
import GFConcepts
import UD2GF

main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  if Help `elem` flags 
    then putStrLn help >> exitSuccess
    else do 
      case args of
        [apref] -> do 
          -- TODO: functionalize
          let epref = fromMaybe "grammars/Extract" (listToMaybe [e | ExtractionGrammar e <- flags])
          let mpref = fromMaybe "MorphoDict" (listToMaybe [m | MorphoDicts m <- flags])
          let opref = fromMaybe "grammars/Generated" (listToMaybe [o | Path o <- flags])
          generateGrammar apref epref mpref opref 
        _ -> do
          putStrLn "Wrong number of arguments" 
          putStrLn help 
          exitWith (ExitFailure 1)

-- type synonyms for the various prefixes
type ExtrPref = FilePath        -- extraction grammar prefix
type OutPref = FilePath         -- generated grammar prefix (optional)
type MDPref = FilePath          -- morphodicts prefix
type AlignedPref = FilePath     -- aligned CoNNL-U files prefix
                                         
-- TODO: return an IO grammar & make main write files?
generateGrammar :: AlignedPref -> ExtrPref -> MDPref -> OutPref -> IO ()
generateGrammar ap ep mp op = do

  -- GETTING PATHS FROM PREFIXES
  aps <- conlluPaths ap    -- paths to the CoNNL-U files
  langs <- conlluLangs aps -- languages to use (alignments langs)
  egps <- gfPaths ep langs -- paths to the modules of the extraction grammar
  mdps <- mdPaths mp langs -- (pairs of) paths to the morphodicts

  -- PARSING & COMPILATION
  us <- mapM parseUDFile aps <&> getAlignments :: IO [[UDSentence]]
  let us' = map (map normalizeCase) us 
  eg <- compileToPGF noOptions egps
  mds <- mapM (compileToPGF noOptions . (\(a,b) -> [a,b])) mdps

  -- TREE CONVERSIONS
  -- 1. gf-ud's UD -> gf-ud's GF 
  udEnvs <- mapM (flip (getEnv ep) "Utt" . show) langs
  let as = zipWith (map . uds2ast) udEnvs us'
  let as' = rmBackups $ transpose as -- rm Backups
  -- 2. gf-ud's GF to GF's GF
  let es = map (map abstree2expr) as'

  -- RULES GENERATION
  let les = map (zip langs) es :: [[(Language,Expr)]]
  env <- getGrammarEnv eg mds op
  let rs = map (tree2rules env) les

  -- RULES POSTPROCESSING
  -- rm duplicatesrules with non-alphanumeric names
  let rs' = nubBy (\r1 r2 -> funname r1 == funname r2) (filter (all isAlpha' . show . funname) rs)
  -- rm pronoun stuff and print to the various files
  let allGrLines = filter (not . isPron) (lines $ prBuiltGrammar env rs')
  let (a:as) = filter (" -- Abstr" `isSuffixOf`) allGrLines 
  let absGrLines = a:"flags startcat = Utt ;":as -- lines of (abstract) Extracted.gf
  let langGrLines = map ((\l -> filter ((" -- " ++ l) `isSuffixOf`) allGrLines) . show) langs -- lines of (concrete) ExtractedLang.gf
  mapM_ 
      (\(l,g) -> writeFile (op ++ l ++ ".gf") g) 
      (("":map show langs) `zip` map unlines (absGrLines:langGrLines))
  where 
    isAlpha' c = isAlpha c || c == '_'
    isPron r = "Pron" `isInfixOf` r
    rmBackups = filter (not . any hasBackup) 
      where hasBackup a = any isBackupFunction (allNodesRTree a)

{- Paths (& languages) from prefixes -}

-- | Given a prefix, return the paths to the aligned CoNNL-U files
conlluPaths :: AlignedPref -> IO [FilePath]
conlluPaths ap = do
  let splitap = splitPath ap
  let dir = concat $ init splitap
  let pre = last splitap
  ls <- listDirectory dir
  return $ sort $
    map (combine dir) (filter (\p -> pre `isPrefixOf` p && isConllu p) ls)

-- | Given a prefix and a list of languages, return the paths of the relevant
-- files of the extraction grammar
gfPaths :: ExtrPref -> [Language] -> IO [FilePath]
gfPaths ep langs = do
  let abs = ep ++ ".gf"
  let gfs = abs:map ((\l -> ep ++ l ++ ".gf") . show) langs
  gfse <- mapM doesFileExist gfs
  if and gfse
    then return $ sort gfs
    else do
      putStrLn "Missing concrete syntax for one or more languages"
      exitWith (ExitFailure 1)

-- | Given a prefix and a list of languages, return a list of pairs 
-- (MorphoDictLanAbs,MorphoDictLan) of the relevant GF morphological
-- dictionaries
mdPaths :: ExtrPref -> [Language] -> IO [(FilePath,FilePath)]
mdPaths mp langs = do
  let as = map ((\l -> mp ++ l ++ "Abs.gf") . show) langs
  let cs = map ((\l -> mp ++ l ++ ".gf") . show) langs
  let mds = as ++ cs -- just for easier existence checking
  mdse <- mapM doesFileExist mds
  if and mdse
    then return $ sort $ as `zip` cs
    else do
      putStrLn "Missing morphodict for one or more languages"
      exitWith (ExitFailure 1)

-- | Given a list of paths to aligned CoNNL-u files, check that they contain
-- valid language ids and return the corresponding list of languages
conlluLangs :: [FilePath] -> IO [Language]
conlluLangs conllus = do
  let langIds = map (reverse . take 3 . reverse . dropExtension) conllus
  let langs = mapMaybe readLanguage langIds
  if length conllus == length langs 
    then return $ sort langs 
    else do
      putStrLn "One or more CoNNL-u files named incorrectly"
      putStrLn help 
      exitWith (ExitFailure 1)


{- Tree conversions -}

-- | Convert a UD sentence into the best corresponding GF AST
uds2ast :: UDEnv -> UDSentence -> AbsTree 
uds2ast env uds = head $ map (expandMacro env) (devtree2abstrees 
                                                $ addBackups            
                                                $ head                  
                                                $ splitDevTree          
                                                $ combineTrees env
                                                $ analyseWords env
                                                $ udtree2devtree
                                                $ udSentence2tree uds)

{- Misc helper functions -}

normalizeCase :: UDSentence -> UDSentence
normalizeCase s = if udUPOS w == "PNOUN" then s else s {
  udWordLines = w { udLEMMA = [toLower c | c <- udLEMMA w]}:ws
}
  where 
    (w:ws) = udWordLines s

-- | Check that a file is in CoNNL-U format (by its extension)
isConllu :: FilePath -> Bool
isConllu p = takeExtension p == ".conllu"

-- | Given n lists of UD sentences obtained via CP and/or CE (in general, with 
-- aligned by sent_id), only get the concepts found in all n langs (so that
-- the lists in the resulting sentences are also aligned implicitly) 
getAlignments :: [[UDSentence]] -> [[UDSentence]]
getAlignments ss =  
  transpose $ mapMaybe getAlignment [(o + 1)..(o + maximum (map length ss))]
    where
      getAlignment :: Int -> Maybe [UDSentence]
      getAlignment i = 
        let as = map (find (\s -> sentId s == i)) ss
        in if all isJust as then Just $ map fromJust as else Nothing
      o = 1000000 -- numbering offset (cf. prUDSentence in UDConcepts)

-- | Return the id of a sentence, taken from the comment that precedes it
-- assumption: the CoNLL-U files are generated by ExtractConcepts and
-- PropagateConcepts and have not been modified
sentId :: UDSentence -> Int
sentId = read . drop 4 . last . words . head . udCommentLines


{- Grammar rules generation -}

data GrammarEnv = GrammarEnv {
  absname :: CId,
  syntaxpgf :: PGF,
  absbasemodules :: [String],
  langenvs :: M.Map Language LangEnv -- lookup with language id
}

-- | Concrete syntax environment
data LangEnv = LangEnv {
  cncname  :: CId,
  dictpgf  :: PGF,
  basemodules :: [String],    -- to be extended
  resourcemodules :: [String] -- to be opened
}

-- | Given the pgf of the extraction grammar, the pgfs of the morphological
-- dictionaries and the prefix of the grammar to be generated, return
-- the environment of the generated grammar
getGrammarEnv :: PGF -> [PGF] -> OutPref -> IO GrammarEnv
getGrammarEnv eg ms op = do
  let egName = show $ abstractName eg -- name of the extraction grammar
  let ggName = takeBaseName op
  return $ GrammarEnv {
    absname = mkCId ggName,  --- hard-coded name of generated module
    syntaxpgf = eg,
    absbasemodules = [egName], --- extending the syntax module
    langenvs = M.fromList [
      (mkCId lang,
       LangEnv {
         cncname = mkCId (ggName ++ lang),
         dictpgf = m,
         basemodules = [egName ++ lang], --- extending the syntax module
         resourcemodules = [name ++ lang, "Paradigms" ++ lang, "MakeStructural" ++ lang]
         }) |
              m <- ms,
              let cname = reverse $ drop 3 $ reverse $ show $ abstractName m,
              let (name,lang) = splitAt (length cname - 3) cname 
       ]
    }

newtype Signature = Signature [Cat]
   deriving Eq

instance Show Signature where
  show (Signature cats) = unwords $ intersperse "->" (map show cats)

returnType :: Signature -> Cat
returnType (Signature cats) = head cats

data BuiltRules = BuiltRules {
  funname  :: CId,
  linrules :: [(Language,(String,Signature))],   -- funs/lins
  unknowns :: [(Language,[(String,Signature)])]  -- opers
} deriving Show

-- | Given the to-generate grammar environment and a n-lingual "concept", 
-- generate the corresponding GF rules
tree2rules :: GrammarEnv -> [(Language,Tree)] -> BuiltRules
tree2rules env lts = BuiltRules {
  funname = fun,
  linrules = [(lang, (linrule lang tree, cat)) | (lang,tree) <- lts, (cat,_) <- [signature lang (rootfun tree)]],
  unknowns = [(lang, unknown lang tree) | (lang,tree) <- lts] -- oper (when something is not found)
}
  where
    -- construct function name (e.g. come_komma_Utt) using the lemmas in all
    -- n langs and the category in the first language
    fun = 
      mkFun (concatMap (init . partsOfFun) (concatMap (lexitems . snd) lts))
             (returnType $ fst (signature firstlang (rootfun firsttree)))

    signature :: Language -> CId -> (Signature,Int)
    signature l f = case functionType synpgf f of
      Just ty -> case unType ty of -- 0: func found in the extraction grammar
        (_,cat,_) -> (Signature [cat],0)
      _ -> case functionType (dictpgf (envoflang l)) f of
        Just ty -> case unType ty of -- 1: func found in the morphodict
          (_,cat,_) -> (Signature [cat],1)
        _ -> (Signature [mkCId $ last (partsOfFun f)],2) -- 2: func not found

    unknown l t = [(showCId f, c) |
      f <- lexitems t,
      (c,2) <- [signature l f]
      ]

    linrule lang tree = showExpr [] tree
    lexitems t = leavesRTree (expr2abstree t)
    rootfun t = root (expr2abstree t)
    synpgf = syntaxpgf env
    (firstlang,firsttree) = head lts
    envoflang l = fromMaybe (error ("unknown lang " ++ show l)) $ M.lookup l (langenvs env)

-- | Print generated rules corresponding to a concept
prBuiltRules :: BuiltRules -> String
prBuiltRules br = unlines $ [
  unwords ["fun",show $ funname br,":",show cat,";","--", unwords (map show cats),"--","Abstr"]
  ] ++ [
  mark c (unwords ["lin",show $ funname br,"=",lin,";","--",show lang]) | (lang,(lin,c)) <- linrules br
  ] ++ [
  unwords ["oper",fun,"=","mk"++show cat, word fun,";","--",show lang] | (lang,funcats) <- unknowns br, (fun,cat) <- funcats
  ]
 where
   word f = "\"" ++ takeWhile (/='_') f ++ "\""
   cat:cats = nub (map (snd . snd) (linrules br))
   mark c s = if c==cat then s else "--- " ++ s -- comment out rule if the category is not the same in both languages

-- | Given the environment of a grammar and its rules, print the entire
-- grammar (abstract and concrete syntaxes mixed together, will be 
-- distinguished by their final comment)
prBuiltGrammar :: GrammarEnv -> [BuiltRules] -> String
prBuiltGrammar env ruless = unlines $ [
   unwords ["abstract", absn, "=",
            intercalate "," (depath (absbasemodules env)), "**","{","-- Abstr"] 
   ] ++ [
   unwords ["concrete", showCId (cncname lenv), "of", absn, "=",
            intercalate ", " (depath (basemodules lenv)), "**",
            "open", intercalate ", " (depath (resourcemodules lenv)), "in","{","--", show lang]
     | (lang,lenv) <- M.assocs (langenvs env) 
   ] ++
   map prBuiltRules ruless ++ [
  "} -- " ++ lang | lang <- "Abstr" : map show langs
   ]
 where
   absn = showCId (absname env)
   langs = M.keys (langenvs env)
   depath modules = map takeFileName modules

-- dependency labels of verb arguments paired with the corresponding Cat
verbargs :: [(Label,Cat)]
verbargs = [
    ("nsubj",mkCId "NP"),
    ("csubj",mkCId "Cl"),
    ("obj",mkCId "NP"),
    ("obl",mkCId "PP"),
    ("iobj",mkCId "NP"),
    ("ccomp",mkCId "Cl"),
    ("xcomp",mkCId "Cl")
  ]

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['e'] ["extraction-grammar"]                          (ReqArg ExtractionGrammar "PREFIX")  "prefix of the extraction grammar, e.g. grammars/Extract."
 , Option ['m'] ["morphodicts", "morphological-dictionaries"]   (ReqArg MorphoDicts "PREFIX")        "prefix common to all the morphological dictionaries to be used, e.g. grammars/Morphodict." 
 , Option ['a'] ["alignments"]                                  (ReqArg Alignments  "PREFIX")        "prefix common to all the aligned CoNNL-U files to be used, e.g. alignments/aligned." 
 , Option ['o'] ["output", "generated-grammar"]                 (ReqArg Path "PREFIX")               "prefix of the generated (output) grammar, e.g. grammars/Generated." 
 , Option ['h'] ["help"]                                        (NoArg Help)                         "show this help message"
 ]

help :: String
help = usageInfo
        ("Usage: stack exec -- generate-grammar -e extraction_grammar_prefix -m morphodicts_prefix -a alignments_prefix [-l languages_to_use] [-o generated_grammar_prefix]."
        ++ "The generated grammar is by default named Generated."
        ++ "NOTE: alignments should follow the same naming conventions applying to GF grammars: they should only differ in the final lang id (e.g. AlignedEng).")
        options