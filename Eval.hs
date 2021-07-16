module Eval where

import Data.List
import Data.Set (size, toList)
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
        [n1,n2] -> do
          algns <- getAlignmentsFromCoNNLUFiles n1 n2
          algns' <- if all isAnnotated algns
                      then return algns
                      else annotate [] algns
          writeFile ("annotated_" ++ n1) (unlines $ map (\(s,n) -> (prUDSentence n (fst $ alignment2sentencePair s))) (algns' `zip` [1..]))
          writeFile ("annotated_" ++ n2) (unlines $ map (\(s,n) -> (prUDSentence n (snd $ alignment2sentencePair s))) (algns' `zip` [1..]))
          return algns'
        [cmd,o1,o2,n1,n2] -> do
          olds <- getAlignmentsFromCoNNLUFiles o1 o2
          news <- getAlignmentsFromCoNNLUFiles n1 n2
          news' <- if all isAnnotated news 
                    then return news 
                    else annotate olds news
          case cmd of
            "extraction" -> putStrLn $ diffStats olds news'
            "propagation" -> putStrLn $ propStats olds news'
          writeFile ("annotated_" ++ n1) (unlines $ map (\(s,n) -> (prUDSentence n (fst $ alignment2sentencePair s))) (news' `zip` [1..]))
          writeFile ("annotated_" ++ n2) (unlines $ map (\(s,n) -> (prUDSentence n (snd $ alignment2sentencePair s))) (news' `zip` [1..]))
          return news'
        _ -> do
          putStrLn "Wrong number of arguments."
          putStrLn help
          exitWith (ExitFailure 1)
      let stats = 
            basicStats as 
            ++ if Reasons `elem` flags then reasonStats as else "" 
      putStrLn stats

{- Annotation -}

-- | Check if an alignment is already annotated
isAnnotated :: Alignment -> Bool
isAnnotated (_,m) = isJust $ correctness m

-- | Annotate the alignments contained in a file, keeping the information
-- provided by an older annotated file into account
annotate :: [Alignment] -> [Alignment] -> IO [Alignment]
annotate olds [] = return []
annotate olds (new:news) = do
  new' <- case trees new `elemIndex` map trees olds of
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

{- Statistics -}

-- | Compare two (annotated) .ca "files" (used to evaluate CE)
diffStats :: [Alignment] -> [Alignment] -> String
diffStats olds news = unlines
  [showLength (filter isCorrect lost) ++ " correct alignments lost",
   showLength (filter isIncorrect lost) ++ " incorrect alignments lost",
   showLength (filter isCorrect found) ++ " correct alignments found",
   showLength (filter isIncorrect found) ++ " incorrect alignments found"]
  where 
    lost = olds \\ news
    found = news \\ olds

-- | Compare two (annotated) .ca "files" (used to evaluate CP)
propStats :: [Alignment] -> [Alignment] -> String
propStats origs props = unlines
  [
    showLength props ++ "/" ++ showLength origs ++ " (" 
    ++ show (listPercent props origs) ++ "%) concepts propagated"
    ++ "\n  of which " ++ showLength correct ++ " (" 
    ++ show (listPercent correct props) ++ "%) correctly"
    ++ "\n  of the " ++ showLength incorrect ++ " (" 
    ++ show (listPercent incorrect props) ++ "%)"
    ++ " incorrect alignments, " ++ showLength newIncorrect ++ " (" 
    ++ show (listPercent newIncorrect incorrect) ++ "%) introduce alignment "
    ++ "errors that are not present in the extracted concepts."
  ]
  where
    correct = filter isCorrect props
    incorrect = filter isIncorrect props
    newIncorrect = filter isNewError incorrect
      where 
        -- weak check!
        isNewError e = isCorrect orig
          where orig = fromJust $ 
                  find (\a -> tl a == sl e || sl a == tl e) origs

-- | Stats for a single (annotated) .ca file
basicStats :: [Alignment] -> String
basicStats alignments = 
  "\ntotal number of alignments extracted: " ++ show totAligns
  ++ "\n  of which " ++ showLength distinct ++ " distinct"
  ++ "\n    of which " ++ showLength correct 
  ++ " (" ++ show correctRatio ++ "%) correct"
  ++ "\n    of which " ++ showLength useful 
  ++ " (" ++ show usefulRatio ++ "%) potentially useful!" ++ "\n" 
  where 
    totAligns = sum $ map (size . sentIds . meta) alignments
    -- filterings
    distinct = nubBy (\a b -> trees a == trees b) alignments
    correct = filter isCorrect distinct
    useful = filter isUseful correct
    incorrect = alignments \\ correct
    -- percentages
    correctRatio = listPercent correct alignments
    usefulRatio = listPercent useful alignments

reasonStats :: [Alignment] -> String
reasonStats alignments = concatMap reasonStats' (powerlist rList)
  where
    -- list of reasons (obtained automagically)
    rList = enumFrom minBound :: [Reason] 
    -- reason-based stats 
    reasonStats' :: [Reason] -> String
    reasonStats' r = if rRatio r /= 0
      then "\n" ++ show r ++ " based alignments: " ++ show (rRatio r) ++ "%" 
           ++ "\n  of which " ++ show (rCorrectRatio r) ++ "% correct"
      else ""
    becauseOf r = filter (isBecauseOf r) alignments
    rRatio r = listPercent (becauseOf r) alignments
    rCorrectRatio r = listPercent (filter isCorrect (filter isCorrect becauseOfR)) becauseOfR
      where becauseOfR = becauseOf r
    powerlist :: [a] -> [[a]]
    powerlist [] = [[]]
    powerlist (x:xs) = [x:ps | ps <- powerlist xs] ++ powerlist xs

-- | Check if alignment is marked as correct (+ or =)
isCorrect :: Alignment -> Bool
isCorrect a = fromJust (correctness (meta a)) `elem` [Specific, Correct]

-- | Check if alignment is marked as incorrect (-)
isIncorrect :: Alignment -> Bool
isIncorrect = not . isCorrect

-- | Check if alignment is marked as useful 
-- (i.e. correct and not too context specific)
isUseful :: Alignment -> Bool
isUseful a = fromJust (correctness (meta a)) == Correct

-- | Check if an alignment has been found because of the given reasons
isBecauseOf :: [Reason] -> Alignment -> Bool
isBecauseOf rs a = toList (reasons $ meta a) == rs

-- | Helper to show lengths
showLength :: [a] -> String
showLength = show . length

-- | Helper to compute %s of alignments from lists
listPercent :: [a] -> [a] -> Float
listPercent n d = 
  (fromIntegral (length n) / fromIntegral (length d)) * 100


{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["reasons"] (NoArg Reasons) "also show reason-wise stats"
 , Option ['h'] ["help"]    (NoArg Help)    "show this help message"
 ]

help :: String
help = usageInfo 
        ("Usage: stack exec -- EvAlign SL.conllu TL.conllu [flags], or\n"
     ++ "        stack exec -- EvAlign extraction annotatedSL.conllu annotatedTL.conllu newSL.conllu newTL.conllu [flags], or"
     ++ "        stack exec -- EvAlign propagation annotatedSL.conllu annotatedTL.conllu newSL.conllu newTL.conllu [flags] "
     ++ "(NOTE: SL.conllu, TL.conllu and newXX.conllu do not need to be pre-annotated)")
        options