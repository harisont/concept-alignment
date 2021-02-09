module Utils where

import Data.List
import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- best to move the debugging functions here
-- to avoid replicating this in every module
import Debug.Trace
-- the trace functions have been modified to accepted variable names as arguments 
tracePrS   varname v = v -- trace ("#" ++ varname ++ "\t" ++ show v) v
tracePrSub varname v = v -- trace ("\t" ++ "#--" ++ varname ++ "\t" ++ show v) v
tracePrL   varname v = v -- trace ("#" ++ varname ++ "\t" ++ show v ++ "\n\n") v
traceMsgPr mesg    v = v -- trace ("#" ++ mesg ++ " " ++ show v) v


-- Moved from GetConfig.hs
splitWithSep :: Eq a => a -> [a] -> [[a]]
splitWithSep sep xs = case break (==sep) xs of
  (x1,_:x2) -> x1 : splitWithSep sep x2
  (x1,[]) -> x1 : []

-- sequence to lattice to enumerate sequences list
replaceAll :: Ord a => M.Map a [a] -> [a] -> [[a]] 
replaceAll dict ws = case ws of
  []   -> []
  h:[] -> [[x] | x <- replh]
  h:_  -> [x:y | x <- replh, y <- replhs]
  where
    replh  = fromMaybe [head ws] $ M.lookup (head ws) dict
    replhs = replaceAll dict (tail ws) 

-- Read the morph feature string and return 
-- MorphoTags / [MorphoConstraints]
str2fsList :: Char -> Char -> String -> String -> [(String,String)]
str2fsList lsep psep def str = case (str == def) of
  True  -> []                          -- default value found. return Empty list
  False -> map (i2pair psep) $ splitWithSep lsep str
  where
    i2pair sep mf = case break (==sep) mf of
     (fn, sep:fv) -> (fn, fv)
     (fn, "")     -> (fn, "")

lpairs2Str :: Char -> Char -> String -> [(String,String)] -> String
lpairs2Str lsep psep def lst = case lst of 
  [] -> def
  _  -> concat . intersperse (lsep:[]) $ map (\(x,y) -> x++(psep:y)) lst

-- cousin of groupSortOn in Data.List.Extra
-- also returns the key 
groupSortByWithKey ::  Ord b => (a -> b) -> [a] -> [(b, [a])]
groupSortByWithKey f xs = map (\zs -> (fst (head zs),map snd zs)) .            -- extract key and values 
                          groupBy (\(k1,_) (k2,_) -> k1 == k2) .               -- group based on key
                          sortBy (\(k1,_) (k2,_) -> compare k1 k2) $           -- sort  based on key
                          zip (map f xs) xs                                    -- zip custom key and value

readInt :: String -> Int
readInt s = case s of
  _:_ | all isDigit s -> read s
  _ -> error $ "failed to read Int from " ++ s
  
quote s = "\"" ++ s ++ "\""  ---- TODO escape

-- only if length s >= 3. 
unquote s       | s == "\"" = s      -- if string itself is quotes 
unquote s@(q:w) | q == '\"' && last w == '\"' = init w 
unquote s@(q:w) | otherwise = s


