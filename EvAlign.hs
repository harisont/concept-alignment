module EvAlign where
import System.Environment (getArgs)
import System.Console.GetOpt
import System.Exit
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import ConceptAlignment
import UDConcepts
import ArgvParse

-- | Horrible evaluation script.
--   Usage: runghc EvAlign labelled_old.ca new.ca 
--   to complete annotation and compare the new alignments with the old ones
-- , or runghc EvAlign labelled.ca to get stats on a single file
main = do
  argv <- getArgs
  (flags,args) <- parseArgv argv help options
  if Help `elem` flags 
    then putStrLn help >> exitSuccess 
    else do
      alignments <- case args of
        [p] -> readFile p >>= return . map parseLine . nonEmptyLines 
        (cmd:p1:p2:_) -> do
          olds <- readFile p1 >>= return . nonEmptyLines 
          news <- readFile p2 >>= return . nonEmptyLines
          let olds' = map parseLine olds
          -- weak annotation check
          news' <- if head (head news) `elem` ['+','-','=']
            then return (map parseLine news)
            else annotate (reverse olds') (reverse $ map read news :: [LinAlignment])
          case cmd of
            "extraction" -> putStrLn $ diffStats olds' news'
            "propagation" -> putStrLn $ propStats olds' news'
          writeFile p2 (unlines $ map (\(x,y) -> show x ++ show y) (reverse news'))
          return news'
        _ -> do 
          putStrLn "Wrong number of arguments."
          putStrLn help
          exitWith (ExitFailure 1)
      let stats = basicStats alignments ++ if Reasons `elem` flags then reasonStats alignments else "" 
      putStrLn stats
    where nonEmptyLines = filter (not . null) . lines

-- | Parse a line of a .ca annotated file 
-- (not trivial because files coming from older versions may not have reasons 
-- or occurrence counts)
parseLine :: String -> AnnotatedAlignment
parseLine s = (a, la)
  where 
    a = read [head s] :: Annotation
    s' = tail s
    [t,u] = splitOn "|" $ takeWhile (/='[') s'
    r = (read $ takeWhile (/=']') (dropWhile (/='[') s') ++ "]") :: [Reason]
    la
      -- complete 
      | '[' `elem` s' && (not . null) (tail $ dropWhile (/=']') s') =
          read s' :: LinAlignment
      -- has reasons but not counts
      | '[' `elem` s' = LAlignment (t,u) r 1
      -- has no reasons nor counts (v1)
      | otherwise = LAlignment (t,u) [] 1

-- | Check if alignment is marked as correct (+ or =)
isCorrect :: AnnotatedAlignment -> Bool
isCorrect (a,_) = a `elem` [Specific, Correct]

-- | Check if alignment is marked as incorrect (-)
isIncorrect :: AnnotatedAlignment -> Bool
isIncorrect = not . isCorrect

-- | Check if alignment is marked as useful 
-- (i.e. correct and not too context specific)
isUseful :: AnnotatedAlignment -> Bool
isUseful (a,_) = a == Correct

-- | Check if an alignment has been found because of the given reasons
isBecauseOf :: [Reason] -> AnnotatedAlignment -> Bool
isBecauseOf rs a = lreasons (snd a) == rs

-- | Compare two (annotated) .ca files (used to evaluate CE)
diffStats :: [AnnotatedAlignment] -> [AnnotatedAlignment] -> String
diffStats olds news = unlines
  [showLength (filter isCorrect lost) ++ " correct alignments lost",
   showLength (filter isIncorrect lost) ++ " incorrect alignments lost",
   showLength (filter isCorrect found) ++ " correct alignments found",
   showLength (filter isIncorrect found) ++ " incorrect alignments found"]
  where 
    lost = olds \\ news
    found = news \\ olds

propStats :: [AnnotatedAlignment] -> [AnnotatedAlignment] -> String
propStats origs props = unlines
  [
    showLength props ++ "/" ++ showLength origs ++ " (" 
    ++ show (listPercent props origs) ++ "%) concepts propagated"
    ++ "\n  of which " ++ showLength correct ++ " (" 
    ++ show (listPercent correct props) ++ "%) correctly"
    ++ "\n  of the " ++ showLength incorrect ++ " (" 
    ++ show (listPercent incorrect props) ++ "%)"
    ++ " incorrect alignments, " ++ showLength newIncorrect ++ " (" 
    ++ show (listPercent newIncorrect (map snd incorrect)) ++ "%) introduce alignment "
    ++ "errors that are not present in the extracted concepts."
  ]
  where
    correct = filter isCorrect props
    incorrect = filter isIncorrect props
    newIncorrect = filter isNewError (map snd incorrect)
      where 
        -- weak check!
        isNewError e = isCorrect orig
          where orig = fromJust $ 
                  find (\(a,la) -> 
                    snd (ltrees la) == fst (ltrees e)
                    || fst (ltrees la) == fst (ltrees e)) origs
  
-- | Stats for a single (annotated) .ca file
basicStats :: [AnnotatedAlignment] -> String
basicStats alignments = 
  "\ntotal number of alignments extracted: " ++ show totAligns
  ++ "\n  of which " ++ showLength distinct ++ " distinct"
  ++ "\n    of which " ++ showLength correct 
  ++ " (" ++ show correctRatio ++ "%) correct"
  ++ "\n    of which " ++ showLength useful 
  ++ " (" ++ show usefulRatio ++ "%) potentially useful!" ++ "\n" 
  where 
    totAligns = sum $ map (loccurrences . snd) alignments
    -- filterings
    distinct = nubBy (\a b -> ltrees (snd a) == ltrees (snd b)) alignments
    correct = filter isCorrect distinct
    useful = filter isUseful correct
    incorrect = alignments \\ correct
    -- percentages
    correctRatio = listPercent correct alignments
    usefulRatio = listPercent useful alignments

reasonStats :: [AnnotatedAlignment] -> String
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

-- | Helper to compute %s of alignments from lists
listPercent :: [a] -> [a] -> Float
listPercent n d = 
  (fromIntegral (length n) / fromIntegral (length d)) * 100

-- | Helper to show lengths
showLength :: [a] -> String
showLength = show . length

-- | Annotate the new .ca file
annotate :: [AnnotatedAlignment] -> [LinAlignment] -> IO [AnnotatedAlignment]
annotate olds [] = return []
annotate olds (new:news) = do
  news' <- annotate olds news
  -- reasons and occurrences don't have anything to do with correctness...
  case (ltrees new `elemIndex` map (ltrees . snd) olds, invLtrees new `elemIndex` map (ltrees . snd) olds) of
    (Nothing,Nothing) -> do
      new' <- completeAnnotation new
      return $ new':news'
    -- ...but the reasons for alignment written to the files should be the
    -- newer ones!
    (Just i,_) -> return $ useNewVals (olds !! i):news'
      where 
        useNewVals (ann,algn) = (ann, algn {
          lreasons = lreasons new,
          loccurrences = loccurrences new
          })
    (_,Just i) -> return $ useNewVals (olds !! i):news'
      where 
        useNewVals (ann,algn) = (ann, algn {
          lreasons = lreasons new,
          loccurrences = loccurrences new
          })
    where 
      invLtrees new = (b,a)
        where (a,b) = ltrees new

-- | Refine +- annotations of a .ca file as +=- (not used)
refine :: [AnnotatedAlignment] -> IO [AnnotatedAlignment]
refine [] = return []
refine (a:as) = do
  as' <- refine as
  if isCorrect a 
    then do
      a' <- completeAnnotation (snd a)
      return $ a':as'
    else return $ a:as'

-- | Make the user annotate a new alignment manually
completeAnnotation :: LinAlignment -> IO AnnotatedAlignment
completeAnnotation algn@(LAlignment (t,u) _ _) = do
  print algn
  putStrLn "(annotate with +, = or -:)" 
  a <- getLine
  -- "à" is an alias for "=" and is just bc of IT keyboard layout ergonomics
  if a `elem` ["+", "=", "-", "à"]
    then return (if a == "à" then Specific else read a :: Annotation, algn)
    else completeAnnotation algn

{- Argument parsing -} 

options :: [OptDescr Flag]
options =
 [ Option ['r'] ["reasons"] (NoArg Reasons) "also show reason-wise stats"
 , Option ['h'] ["help"]    (NoArg Help)    "show this help message"
 ]

help :: String
help = usageInfo 
        ("Usage: stack exec -- EvAlign annotated.ca [flags], or\n"
     ++ "        stack exec -- EvAlign extraction annotated.ca new.ca [flags], or"
     ++ "        stack exec -- EvAlign propagation annotated.ca new.ca [flags] "
     ++ "(NOTE: new.ca does not need to be pre-annotated)")
        options