module ExtractConcepts where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import System.FilePath.Posix
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import UDConcepts
import UDPatterns
import ConceptAlignment
--import FastAlignUtils
import Criteria 
import ArgvParse

main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  case args of
    [src,trg] -> if Help `elem` flags
      then putStrLn help >> exitSuccess
      else do
        ts <- parseUDFile src
        us <- parseUDFile trg
        let tus = zip ts us -- let tus = take 50 $ zip ts us
        let smtAs = S.empty -- <- getSmtAlignments flags src trg
        let segment = Clauses `elem` flags
        let byExcl = Rest `elem` flags
        let fp = listToMaybe [path | Path path <- flags] 
        let fp' = fromJust fp
        r <- getPattern flags
        let as = align smtAs criteria r segment byExcl tus
        let m = listToMaybe [read mmax :: Int | MaxSize mmax <- flags]
        let as' = if All `elem` flags then as else selectForMT m as
        let as'' = sortByConfidence (S.toList as') 
        if Linearize `elem` flags
          then 
            if isJust fp 
              then writeFile fp' (unlines $ map prAlignment as'') 
              else mapM_ (putStrLn . prAlignment) as''
          else 
            if isJust fp 
              then do
                let numberedSentPairs = [1..] `zip` map alignment2sentencePair as''
                writeFile (insertLang fp' "SL") (unlines [prUDSentence (fst x) (fst $ snd x) | x <- numberedSentPairs])
                writeFile (insertLang fp' "TL") (unlines [prUDSentence (fst x) (snd $ snd x) | x <- numberedSentPairs]) 
              else mapM_ print as''
    _ -> do
      putStrLn "Wrong number of arguments."
      putStrLn help
      exitWith (ExitFailure 1)
  where 
    insertLang :: FilePath -> String -> FilePath
    insertLang fp l = dropExtension fp ++ l ++ takeExtension fp
    getSmtAlignments = undefined
    --getSmtAlignments :: [Flag] -> FilePath -> FilePath -> IO [LinAlignment]
    --getSmtAlignments flags src trg = case [path | Pharaoh path <- flags] of
    --  [] -> return []
    --  [path] -> do
    --    indices <- readFile path >>= return . parsePh
    --    srcConllu <- readFile src
    --    trgConllu <- readFile trg
    --    let bitext = take 100 $ parseBi $ conllu2bi (srcConllu,trgConllu)
    --    print $ ltrees $ phToLas bitext indices !! 5
    --    return $ phToLas bitext indices
    --  _ -> undefined
    getPattern :: [Flag] -> IO (Maybe UDPattern)
    getPattern flags = do
          let rp = listToMaybe [p | Pattern p <- flags] 
          if isJust rp 
            then do
              patternText <- readFile (fromJust rp)
              return $ Just (read patternText :: UDPattern)
            else return Nothing

-- | Sort alignments by how likely they are to be correct (kinda). 
sortByConfidence :: [Alignment] -> [Alignment]
sortByConfidence = sortOn (\a -> 
                        let rs = S.toList (reasons $ meta a) \\ [HEAD, PREV] 
                        in (
                            -(length rs), 
                            -(length $ sentIds $ meta a), 
                            tail rs
                          )) 

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option []    ["pharaoh"]         (ReqArg Pharaoh "FILE")  "use alignment in pharaoh format as backup"
 , Option ['f'] ["file"]            (ReqArg Path "FILE")     "write the output to a file (two separate CoNLL-u files if the alignments are not linearized)" 
 , Option ['m'] ["maxsize", "max"]  (ReqArg MaxSize "INT")   "set a max size for the extracted alignments"
 , Option ['p'] ["pattern"]         (ReqArg Pattern "FILE")  "look for a specific pattern (.hst) (e.g. all alignments where the head of both trees is a NOUN)"
 , Option ['a'] ["all"]             (NoArg All)              "do not filter out any alignments, regardless what other flags say"
 , Option []    ["clauses"]         (NoArg Clauses)          "align clause-by-clause"
 , Option []    ["rest"]            (NoArg Rest)             "try to align all nominals and modifiers"
 , Option ['l'] ["linearize"]       (NoArg Linearize)        "print out alignments in .ca format instead of trees"
 , Option ['h'] ["help"]            (NoArg Help)             "show this help message"
 ]

help :: String
help = usageInfo 
        "Usage: stack exec -- ExtractConcepts SL.conllu TL.conllu [flags]"
        options