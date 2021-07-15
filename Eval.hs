module Eval where

--import Data.List
import Data.Maybe
import System.Exit
import System.Environment (getArgs)
import System.Console.GetOpt
import UDConcepts
import ArgvParse
import ConceptAlignment

main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  if Help `elem` flags 
    then putStrLn help >> exitSuccess 
    else do
      as <- case args of 
        [n1,n2] -> getAlignmentsFromUDFiles n1 n2
        [cmd,o1,o2,n1,n2] -> do
          olds <- getAlignmentsFromUDFiles o1 o2
          news <- getAlignmentsFromUDFiles n1 n2
          news' <- if all isAnnotated news 
                    then return news 
                    else annotate olds news
          case cmd of
            "extraction" -> putStrLn "TODO:"
            "propagation" -> putStrLn "TODO:"
          writeFile n1 (unlines $ map (show . fst) news')
          writeFile n2 (unlines $ map (show . snd) news')
          return news'
        _ -> do
          putStrLn "Wrong number of arguments."
          putStrLn help
          exitWith (ExitFailure 1)
      putStrLn "TODO:"

type Path = String

getAlignmentsFromUDFiles :: Path -> Path -> IO [Alignment]
getAlignmentsFromUDFiles p1 p2 = do 
  p1' <- parseUDFile p1
  p2' <- parseUDFile p2
  return $ zipWith (curry sentencePair2alignment) p1' p2'


isAnnotated :: Alignment -> Bool
isAnnotated (_,m) = isJust $ correctness m

-- | Annotate the alignments contained in a file, keeping the information
-- provided by an older annotated file into account
annotate :: [Alignment] -> [Alignment] -> IO [Alignment]
annotate olds [] = return []
annotate olds (new:news) = do
  new' <- case new `elemIndex` olds of
    Nothing -> annotateManually new
    Just i -> return (
      trees new,
      (meta new) { 
        correctness = correctness $ meta (olds !! i) 
      })
  news' <- annotate olds news
  return (new':news')
  where 
    annotateManually new = do
      putStrLn $ prLinearizedAlignment new
      putStrLn "(annotate with +, = or -:)" 
      a <- getLine
      -- "à" is an alias for "=" and is just bc of IT keyboard layout ergonomics
      if a `elem` ["+", "=", "-", "à"]
        then return (trees new, (meta new) {
          correctness = if a == "à" 
                          then Just Specific
                          else Just (read a :: Annotation) 
        })
        else annotateManually new

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["reasons"] (NoArg Reasons) "also show reason-wise stats"
 , Option ['h'] ["help"]    (NoArg Help)    "show this help message"
 ]

help :: String
help = usageInfo 
        ("Usage: stack exec -- EvAlign annotatedSL.conllu annotatedTL.conllu [flags], or\n"
     ++ "        stack exec -- EvAlign extraction annotatedSL.conllu annotatedTL.conllu newSL.conllu newTL.conllu [flags], or"
     ++ "        stack exec -- EvAlign propagation annotatedSL.conllu annotatedTL.conllu newSL.conllu newTL.conllu [flags] "
     ++ "(NOTE: newXX.conllu does not need to be pre-annotated)")
        options