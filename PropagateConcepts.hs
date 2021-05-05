module PropagateConcepts where

import Data.List
import Data.Maybe
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import RTree
import UDConcepts
import ConceptAlignment
import Criteria
import ArgvParse

main = do
    argv <- getArgs
    (flags,args) <- parseArgv argv help options
    case args of
        [old,src,trg] -> if Help `elem` flags
            then putStrLn help >> exitSuccess
            else do
                cs <- conlluFile2UDTrees old
                ts <- parseUDFile src
                us <- parseUDFile trg
                let segment = Clauses `elem` flags
                let byExcl = Rest `elem` flags
                let fp = listToMaybe [path | Path path <- flags] 
                let fp' = fromJust fp
                let cs' = map unadjust cs
                let as = map (propagate criteria segment byExcl (ts,us)) cs'
                if Linearize `elem` flags
                    then 
                        if isJust fp 
                            then writeFile fp' (unlines $ map 
                                        (maybe "" prLinearizedAlignment) 
                                        as) 
                            else mapM_ 
                                    (putStrLn . maybe "" prLinearizedAlignment) 
                                    as
                    else 
                        if isJust fp 
                            then do
                                let numberedAs = [1..] `zip` as
                                writeFile fp' (unlines [prUDSentence (fst na) (snd $ alignment2sentencePair $ fromJust (snd na)) | na <- numberedAs, isJust (snd na)])
                            else mapM_ print (filter isJust as) 
        _ -> do
            putStrLn "Wrong number of arguments."
            putStrLn help
            exitWith (ExitFailure 1)
    where
        -- use original label
        unadjust :: UDTree -> UDTree
        unadjust (RTree n ts) = RTree n { 
            udDEPREL = fromMaybe (udDEPREL n) (getOrigLabel n)
        } ts
            where getOrigLabel n = 
                    listToMaybe [head ls | UDData "ORIG_LABEL" ls <- udMISC n]

{- Argument parsing -} 

options :: [OptDescr Flag]
options = [ 
      Option ['f']  ["file"]      (ReqArg Path "FILE")      "write the output to a file" 
    , Option []     ["clauses"]   (NoArg Clauses)           "align clause-by-clause"
    , Option []     ["rest"]      (NoArg Rest)              "try to align all nominals and modifiers"
    , Option ['l']  ["linearize"] (NoArg Linearize)         "print out alignments in .ca format instead of trees"
    , Option ['h']  ["help"]      (NoArg Help)              "show this help message"
    ]

help :: String
help = usageInfo 
        "Usage: stack exec -- PropagateConcepts SL_concepts.conllu SL.conllu TL.conllu [flags]"
        options