module Eval where

import System.Exit
import System.Environment (getArgs)
import System.Console.GetOpt
import ArgvParse
import UDConcepts

-- | Evaluation script
main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  if Help `elem` flags 
    then putStrLn help >> exitSuccess 
    else do
      alignments <- case args of
        [p] -> parseUDFile p 
        (cmd:p1:p2:_) -> do
          olds <- parseUDFile p1 
          news <- parseUDFile p2 
          news' <- if all isAnnotated news
                    then return news
                    else annotate (reverse olds) (reverse news) 
          case cmd of 
            "extraction" -> putStrLn $ diffStats olds news
            "propagation" -> putStrLn $ propStats olds news
          writeFile p2 (unlines $ map prt (reverse news'))
          return news
        _ -> do
          putStrLn "Wrong number of arguments."
          putStrLn help
          exitWith (ExitFailure 1)
      putStrLn $ basicStats alignments
      if Reasons `elem` flags 
        then putStrLn $ reasonStats alignments
        else putStrLn ""

basicStats = undefined
reasonStats = undefined
diffStats = undefined
propStats = undefined

isAnnotated = undefined
annotate = undefined

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["reasons"] (NoArg Reasons) "also show reason-wise stats"
 , Option ['h'] ["help"]    (NoArg Help)    "show this help message"
 ]

help :: String
help = usageInfo 
        ("Usage: stack exec -- EvAlign annotated.conllu [flags], or\n"
     ++ "        stack exec -- EvAlign extraction annotated.conllu new.conllu [flags], or"
     ++ "        stack exec -- EvAlign propagation annotated.conllu new.conllu [flags] "
     ++ "(NOTE: new.conllu does not need to be pre-annotated)")
        options