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
import Data.Maybe
import Data.Char

-- gf
import GF hiding (isPrefixOf, main)
import GF.Support
import PGF

-- gf-ud
import RTree
import GFConcepts
import UDConcepts
import UDAnnotations
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
                                                                                -- TODO: function
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
    eg <- compileToPGF noOptions egps -- pgf extraction grammar                 -- TODO: maybe change opts (path)
    mds <- mapM (compileToPGF noOptions . pair2list) mdps -- pgf morphodicts    -- TODO: maybe change opts (path + name)

    print aps
    print langs
    -- create UD environments (one per language)
    envs <- mapM (flip (getEnv ep) "Utt") (map show langs)
    -- associate envs (langs) to lists of UD sentences
    let ecs = envs `zip` us :: [(UDEnv, [UDSentence])]
    -- transform UD sentences into (LISTS OF, cf. uds2ast) ASTs, 
    -- translation equivalents are on the same row
    let as = map (\(e,ss) -> map (uds2ast e) ss) ecs
    let langNotes = map (++ ": ") (map show langs)
    let aas = map (zip langNotes) (transpose as)
    -- cf. $paste out/gfts-en.tmp out/gfts-it.tmp but with additional formatting required by BuildGrammar
    let stras = map (intercalate "\n" . map (\(a,ts) -> a ++ concatMap prAbsTree ts)) aas
    -- cf. $grep -v Backup | sort -u 
    let stras' = intersperse "\n" $ sort $ nub $ filter (not . isInfixOf "Backup") stras
    -- putStrLn stras' -- just because "se non vedo non credo" 
    -- filter out trees containing weird characters and numbers
    let stras'' = filter (all isAlpha') stras' 
    buildGFGrammar (ep ++ ".pgf") (map (\l -> mp ++ l ++ ".pgf") (map show langs)) (unlines stras'')

isPfg p = takeExtension p == ".pgf"

isAlpha' c = or [isAlpha c, c == ' ', c == '\n', c == '_', c == ':', c == '(', c == ')']

-- | Check that a file is in CoNNL-U format (by its extension)
isConllu :: FilePath -> Bool
isConllu p = takeExtension p == ".conllu"

pair2list :: (a,a) -> [a]
pair2list (a,b) = [a,b]

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
getAlignments sss =  
    transpose $ mapMaybe getAlignment [(offs + 1)..(offs + maximum (map length sss))]
        where
            getAlignment :: Int -> Maybe [UDSentence]
            getAlignment i = 
                let as = map (find (\s -> sentId s == i)) sss
                in if all isJust as then Just $ map fromJust as else Nothing
            offs = 1000000 -- numbering offset (cf. prUDSentence in UDConcepts)

-- | Return the id of a sentence, taken from the comment that precedes it
sentId :: UDSentence -> Int
-- assumption: the CoNLL-U files are generated by ExtractConcepts and
-- PropagateConcepts and have not been modified
sentId = read . drop 4 . last . words . head . udCommentLines

-- | Convert a UD sentence into the best corresponding GF ASTs via gfud 
-- functions (a more general function of this kind should REALLY belong to 
-- gfud!)
-- (NOTE: not used cause ud2gf tends to eat up all memory by getting stuck
-- on specific trees)
uds2ast :: UDEnv -> UDSentence -> [AbsTree] 
uds2ast env uds = map (expandMacro env) (devtree2abstrees 
                                    $ addBackups            
                                    $ head                  
                                    $ splitDevTree          
                                    $ combineTrees env
                                    $ analyseWords env
                                    $ udtree2devtree
                                    $ simpleRoot
                                    $ udSentence2tree uds)

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