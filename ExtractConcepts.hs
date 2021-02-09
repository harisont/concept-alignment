module ExtractConcepts where

import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import UDConcepts
import ConceptAlignment
import FastAlignUtils
import Criteria 
import ArgvParse

main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  case args of
    [src,trg] -> if Help `elem` flags
      then putStrLn help >> exitSuccess
      else do
        ts <- conlluFile2UDTrees src
        us <- conlluFile2UDTrees trg
        let tus = zip ts us -- let tus = take 50 $ zip ts us
        smtAs <- getSmtAlignments flags src trg
        let segment = Clauses `elem` flags
        let byExcl = Rest `elem` flags
        let fp = listToMaybe [path | Path path <- flags] 
        let fp' = fromJust fp
        let as = M.toList $ align smtAs criteria segment byExcl tus
        let m = listToMaybe [read mmax :: Int | MaxSize mmax <- flags]
        let as' = if All `elem` flags then as else selectForMT m as
        let as'' = sortByConfidence as'
        if Linearize `elem` flags
          then 
            if isJust fp 
              then writeFile fp' (unlines $ map (show . toLinAlignment) as'') 
              else mapM_ (print . toLinAlignment) as''
          else 
            if isJust fp 
              then do
                let numberedSentPairs = [1..] `zip` map (alignment2sentencePair . fst) as''
                writeFile ("SL" ++ fp') (unlines [prUDSentence (fst x) (fst $ snd x) | x <- numberedSentPairs])
                writeFile ("TL" ++ fp') (unlines [prUDSentence (fst x) (snd $ snd x) | x <- numberedSentPairs]) 
              else mapM_ (print . fst) as''
    _ -> do
      putStrLn "Wrong number of arguments."
      putStrLn help
      exitWith (ExitFailure 1)
  where 
    getSmtAlignments :: [Flag] -> FilePath -> FilePath -> IO [LinAlignment]
    getSmtAlignments flags src trg = case [path | Pharaoh path <- flags] of
      [] -> return []
      [path] -> do
        indices <- readFile path >>= return . parsePh
        srcConllu <- readFile src
        trgConllu <- readFile trg
        let bitext = take 100 $ parseBi $ conllu2bi (srcConllu,trgConllu)
        print $ ltrees $ phToLas bitext indices !! 5
        return $ phToLas bitext indices
      _ -> undefined

-- | Sort alignments by how likely theyare to be correct (kinda). 
sortByConfidence :: [(Alignment,Info)] -> [(Alignment,Info)]
sortByConfidence = sortOn (\(a,(r,o)) -> 
                        let rs = (S.toList r) \\ [HEAD, PREV] 
                        in (
                            -(length rs), 
                            -(o), 
                            tail rs
                          )) 

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option []    ["pharaoh"]         (ReqArg Pharaoh "FILE")   "use alignment in pharaoh format as backup"
 , Option ['f'] ["file"]            (ReqArg Path "FILE")      "write the output to a file (two separate CoNLL-u files if the alignments are not linearized)" 
 , Option ['m'] ["maxsize", "max"]  (ReqArg MaxSize "INT")    "set a max size for the extracted alignments"
 , Option ['a'] ["all"]             (NoArg All)               "do not filter out any alignments, regardless what other flags say"
 , Option []    ["clauses"]         (NoArg Clauses)           "align clause-by-clause"
 , Option []    ["rest"]            (NoArg Rest)              "try to align all nominals and modifiers"
 , Option ['l'] ["linearize"]       (NoArg Linearize)         "print out alignments in .ca format instead of trees"
 , Option ['h'] ["help"]            (NoArg Help)              "show this help message"
 ]

help :: String
help = usageInfo 
        "Usage: stack exec -- ExtractConcepts SL.conllu TL.conllu [flags]"
        options