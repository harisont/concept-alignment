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
import qualified Data.Map as M
import Data.Maybe
import Data.Char

-- gf
import GF hiding (isPrefixOf, main)
import GF.Support
import PGF

-- gf-ud
import RTree
import UDConcepts
import UDAnnotations
import GFConcepts
import UD2GF
import BuildGFGrammar -- TODO: remove

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
                                         
-- TODO: return an IO grammar, make main write files
generateGrammar :: AlignedPref -> ExtrPref -> MDPref -> OutPref -> IO ()
generateGrammar ap ep mp op = do

    -- FINDING, READING & COMPILING FILES
    aps <- conlluPaths ap    -- paths to the CoNNL-U files
    langs <- conlluLangs aps -- languages to use (alignments langs)
    egps <- gfPaths ep langs -- paths to the modules of the extraction grammar
    mdps <- mdPaths mp langs -- (pairs of) paths to the morphodicts
    us <- mapM parseUDFile aps <&> getAlignments :: IO [[UDSentence]]
    eg <- compileToPGF noOptions egps
    mds <- mapM (compileToPGF noOptions . (\(a,b) -> [a,b])) mdps

    -- WRITING PGFs in the right place (extraction grammar and morphodicts)
    -- (TODO: GF Options didn't seem to work, but they should)
    writePGF noOptions eg
    mapM_ (writePGF noOptions) mds 
    renameFile (takeBaseName ep ++ ".pgf") (ep ++ ".pgf")
    mapM_ (\l -> renameFile (takeBaseName mp ++ show l ++ "Abs.pgf") (mp ++ show l ++ ".pgf")) langs

    -- TODO: review after merge 
    -- create UD environments (one per language)
    envs <- mapM (flip (getEnv ep) "Utt" . show) langs
    -- associate envs (langs) to lists of UD sentences
    let ecs = envs `zip` us :: [(UDEnv, [UDSentence])]
    -- transform UD sentences into ASTs, 
    -- translation equivalents are on the same row
    let as = map (\(e,ss) -> map (uds2ast e) ss) ecs
    let langNotes = map (++ ": ") (map show langs)
    let aas = map (zip langNotes) (transpose as)
    -- cf. $paste out/gfts-en.tmp out/gfts-it.tmp but with additional formatting required by BuildGrammar
    let stras = map (intercalate "\n" . map (\(a,t) -> a ++ prAbsTree t)) aas
    -- cf. $grep -v Backup | sort -u 
    let stras' = intersperse "\n" $ sort $ nub $ filter (not . isInfixOf "Backup") stras
    -- putStrLn stras' -- just because "se non vedo non credo" 
    -- filter out trees containing weird characters and numbers
    let stras'' = filter (all isAlpha') stras' 

    let abstr = ep ++ ".pgf"
    let dicts = map (\l -> mp ++ l ++ ".pgf") (map show langs)
    let als = unlines stras''
    env <- getGrammarEnv abstr dicts
    let aligns = readAlignments als
    let ruless = map (tree2rules env) aligns
    let allGrLines = filter (not . isPron) (lines $ prBuiltGrammar env ruless)
    let (a:as) = filter (" -- Abstr" `isSuffixOf`) allGrLines 
    let absGrLines = a:"flags startcat = Utt ;":as -- lines of (abstract) Extracted.gf
    let langs = map fst (M.toList $ langenvs env)
    let langGrLines = map (\l -> filter ((" -- " ++ l) `isSuffixOf`) allGrLines) langs -- lines of (concrete) ExtractedLang.gf
    mapM_ 
        (\(l,g) -> writeFile (dropFileName abstr ++ "Extracted" ++ l ++ ".gf") g) 
        (("":langs) `zip` map unlines (absGrLines:langGrLines))
    where 
        isPron r = "Pron" `isInfixOf` r

isAlpha' c = or [isAlpha c, c == ' ', c == '\n', c == '_', c == ':', c == '(', c == ')']

-- | Check that a file is in CoNNL-U format (by its extension)
isConllu :: FilePath -> Bool
isConllu p = takeExtension p == ".conllu"

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
-- (MorphoDictLanAbs,MorphoDictLan) of gf morphological dictionaries
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
sentId :: UDSentence -> Int
-- assumption: the CoNLL-U files are generated by ExtractConcepts and
-- PropagateConcepts and have not been modified
sentId = read . drop 4 . last . words . head . udCommentLines

-- | Convert a UD sentence into the best corresponding GF AST via gfud 
-- functions
uds2ast :: UDEnv -> UDSentence -> AbsTree 
uds2ast env uds = head $ map (expandMacro env) (devtree2abstrees 
                                                $ addBackups            
                                                $ head                  
                                                $ splitDevTree          
                                                $ combineTrees env
                                                $ analyseWords env
                                                $ udtree2devtree
                                                $ simpleRoot
                                                $ udSentence2tree uds)


-- TODO:  should be not based on print-read
-- | Convert a gf-ud AbsTree into a GF Expr
ast2expr :: AbsTree -> Expr
ast2expr a = case readExpr (prAbsTree a) of
    Just e -> e
    _ -> error "invalid AbsTree"

-- to ignore any kind of weird label/subtype in the output of CA
simpleRoot :: UDTree -> UDTree
simpleRoot (RTree n ts) = RTree (n { udDEPREL = "root"}) ts

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