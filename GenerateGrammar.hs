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
import Data.List.Split
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
import UDAnnotations hiding (getEnv)
import GFConcepts
import UD2GF

main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  if Help `elem` flags 
    then putStrLn help >> exitSuccess
    else do 
      case args of
        aprefs@(_:_) -> do 
          -- TODO: functionalize
          let epref = fromMaybe "grammars/Extract" (listToMaybe [e | ExtractionGrammar e <- flags])
          let mpref = fromMaybe "MorphoDict" (listToMaybe [m | MorphoDicts m <- flags])
          let opref = fromMaybe "grammars/Generated" (listToMaybe [o | Path o <- flags])
          generateGrammar aprefs epref mpref opref 
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
generateGrammar :: [AlignedPref] -> ExtrPref -> MDPref -> OutPref -> IO ()
generateGrammar aps ep mp op = do

  -- GETTING PATHS FROM PREFIXES
  cps <- mapM conlluPaths aps   -- paths to the CoNNL-U files
  langs <- mapM conlluLangs cps -- languages to use (alignments langs)
  let langs' = foldr1 intersect langs
  egps <- gfPaths ep langs'     -- paths to the modules of the extr. grammar
  mdps <- mdPaths mp langs'     -- (pairs of) paths to the morphodicts

  -- PARSING & COMPILATION
  us <- mapM (mapM parseUDFile) cps 
  let us' = map concat (transpose (map getAlignments us))
  let us'' = map (map normalizeCase) us'
  eg <- compileToPGF noOptions egps
  mds <- mapM (compileToPGF noOptions . (\(a,b) -> [a,b])) mdps

  -- TREE CONVERSIONS
  -- 1. gf-ud's UD -> gf-ud's GF 
  udEnvs <- mapM (flip (getEnv eg ep) "Utt" . show) langs'
  let as = zipWith (map . uds2ast) udEnvs us''
  let as' = rmBackups $ transpose as -- rm Backups
  -- 2. gf-ud's GF to GF's GF
  let es = map (map (\(a,cs) -> (abstree2expr a, cs))) as'

  -- RULES GENERATION
  let les = map (zip langs') es :: [[(Language,(Expr,[String]))]]
  env <- getGrammarEnv eg mds op
  let rs = map (tree2rules env) les

  -- RULES POSTPROCESSING
  -- rm duplicatesrules with non-alphanumeric names
  let rs' = nubBy (\r1 r2 -> funname r1 == funname r2) (filter (all isAlpha' . show . funname) rs)
  -- rm pronoun stuff and print to the various files
  let allGrLines = filter (not . isPron) (lines $ prBuiltGrammar env rs')
  let (a:as) = filter (" -- Abstr" `isSuffixOf`) allGrLines 
  let absGrLines = a:"flags startcat = Utt ;":as -- lines of (abstract) Extracted.gf
  let langGrLines = map ((\l -> filter ((" -- " ++ l) `isSuffixOf`) allGrLines) . show) langs' -- lines of (concrete) ExtractedLang.gf
  mapM_ 
      (\(l,g) -> writeFile (op ++ l ++ ".gf") g) 
      (("":map show langs') `zip` map unlines (absGrLines:langGrLines))
  where 
    isAlpha' c = isAlpha c || c == '_'
    isPron r = "Pron" `isInfixOf` r
    rmBackups = filter (not . any hasBackup) 
      where hasBackup (t,_) = any isBackupFunction (allNodesRTree t)

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

-- | Convert a UD sentence into the best corresponding GF AST, 
-- keeping the comment lines
uds2ast :: UDEnv -> UDSentence -> (AbsTree,[String]) 
uds2ast env uds = (head $ map (expandMacro env) (devtree2abstrees 
                                                $ addBackups            
                                                $ head                  
                                                $ splitDevTree          
                                                $ combineTrees env
                                                $ analyseWords env
                                                $ udtree2devtree
                                                $ udSentence2tree uds),
                  udCommentLines uds)

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
  transpose $ mapMaybe getAlignment ids
    where
      ids = nub $ map sentId (concat ss)
      getAlignment :: String -> Maybe [UDSentence]
      getAlignment id = 
        let as = map (find (\s -> sentId s == id)) ss
        in if all isJust as then Just $ map fromJust as else Nothing

-- | Like UDAnnotations.getEnv, but uses pgf instead of paths
-- (this is a bad solution but I don't necessarily want to rely on the pgf
-- to be in a certain location)
getEnv :: PGF -> String -> String -> String -> IO UDEnv
getEnv pgf pref eng cat = do
  abslabels <- readFile (stdAbsLabelsFile pref) >>= return . pAbsLabels
  cnclabels <- readFile (stdCncLabelsFile pref eng) >>= return . pCncLabels
  let actlang = stdLanguage pref eng
  let env = mkUDEnv pgf abslabels cnclabels actlang cat
  return $ addMissing env

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
    absname = mkCId ggName,
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
returnType (Signature cats) = last cats

data BuiltRules = BuiltRules {
  funname  :: CId,
  linrules :: [(Language,(String,Signature))],   -- funs/lins
  unknowns :: [(Language,[(String,Signature)])],  -- opers
  ids :: [String]
} deriving Show

-- | Given the to-generate grammar environment and a n-lingual "concept", 
-- generate the corresponding GF rules
tree2rules :: GrammarEnv -> [(Language,(Tree,[String]))] -> BuiltRules
tree2rules env ltss = BuiltRules {
  funname = fun,
  linrules = [(lang, (linrule lang tree, cat)) | (lang,tree) <- lts, (cat,_) <- [signature lang tree]],
  unknowns = [(lang, unknown lang tree) | (lang,tree) <- lts], -- oper (when something is not found)
  ids = sentIds cs -- the sent_ids of the sentences the rule was derived from
}
  where
    cs = map (\(_,(_,s)) -> s) ltss -- comment lines 
    sentIds = nub . concatMap (readSet . head) . tail . splitOn ["sentence","IDs:"] . words . unwords . concat 
      where readSet s = splitOn "," $ filter (\c -> c `notElem` ['{','}', '\"']) s
    lts = map (\(l,(t,_)) -> (l,t)) ltss 
    -- construct function name (e.g. come_komma_Utt) using the lemmas in all
    -- n langs and the category in the first language
    fun = 
      mkFun (concat $ concatMap (map (\x -> if show x == "__" then [""] else init $ partsOfFun x)) (intersperse [mkCId "__"] (map (lexitems . snd) lts)))
             (returnType $ fst (signature firstlang firsttree))

    signature :: Language -> Tree -> (Signature,Int)
    signature l t = case functionType synpgf f of
      Just ty -> case unType ty of -- 0: func found in the extraction grammar
        (_,cat,_) -> (Signature (paramTypes ++ [cat]),0)
      _ -> case functionType (dictpgf (envoflang l)) f of
        Just ty -> case unType ty of -- 1: func found in the morphodict
          (_,cat,_) -> (Signature (paramTypes ++ [cat]),1)
        _ -> (Signature (paramTypes ++ [mkCId $ last (partsOfFun f)]),2) -- 2: func not found
      where 
        f = rootfun t
        paramTypes = catMaybes [
              if label `isInfixOf` show t then Just cat else Nothing 
              | (label,cat) <- verbargs]

    unknown l t = [(showCId f, c) |
      f <- lexitems t,
      (c,2) <- if any (`isPrefixOf` (show f)) (map fst verbargs) then [] else [signature l (abstree2expr (RTree f []))]
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
  unwords ["fun",fullname br,":",show $ head cats,";","--","Abstr"]
  ] ++ [
  mark c (unwords ["lin",fullname br,unwords paramNames,"=",lin' lin,";","--",show lang]) | (lang,(lin,c)) <- lins
  ] ++ [
  unwords ["oper",fun,"=","mk"++show cat, word fun,";","--",show lang] | (lang,funcats) <- unknowns br, (fun,cat) <- funcats
  ]
 where
  fullname br = intercalate "_" (ids br) ++ "__" ++ show (funname br)
  word f = "\"" ++ takeWhile (/='_') f ++ "\""
  cats = nub (map (snd . snd) lins)
  mark c s = if all (==c) cats then s else "--- " ++ s -- comment out rule if the signature is not the same in both languages (for the moment at least)
  lins = linrules br
  paramNames = ['p':show n | n <- [1..length $ tail params]]
    where (Signature params) = snd $ snd $ head lins
  lin' lin = unwords $ replace (words lin) paramNames
  -- replace leaves with param names
  replace (w:ws) pars@(p:ps) = 
    if any ((`isPrefixOf` w) . fst) verbargs 
      then (p++brackets):replace ws ps 
      else w:replace ws pars
    where brackets = takeWhile (== ')') $ reverse w
  replace ws [] = ws 
  replace [] _ = []

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
    ("nsubj",mkCId "N"),
    ("csubj",mkCId "Cl"),
    ("obj",mkCId "N"),
    ("obl",mkCId "N"),
    ("iobj",mkCId "N"),
    ("ccomp",mkCId "Cl"),
    ("xcomp",mkCId "Cl")
  ]

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['e'] ["extraction-grammar"]                          (ReqArg ExtractionGrammar "PREFIX")  "prefix of the extraction grammar, e.g. grammars/Extract."
 , Option ['m'] ["morphodicts", "morphological-dictionaries"]   (ReqArg MorphoDicts "PREFIX")        "prefix common to all the morphological dictionaries to be used, e.g. grammars/Morphodict." 
 , Option ['o'] ["output", "generated-grammar"]                 (ReqArg Path "PREFIX")               "prefix of the generated (output) grammar, e.g. grammars/Generated." 
 , Option ['h'] ["help"]                                        (NoArg Help)                         "show this help message"
 ]

help :: String
help = usageInfo
        ("Usage: stack exec -- generate-grammar alignments_prefixes [flags]."
        ++ "\nThe generated grammar is by default named Generated."
        ++ "\nNOTE: alignments should follow the same naming conventions applying to GF grammars: they should only differ in the final lang id (e.g. AlignedEng).")
        options