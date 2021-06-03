module FastAlignUtils where

import System.Environment (getArgs)
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import RTree
import UDConcepts
import ConceptAlignment

-- | Parse a string in pharaoh format to a list of one-to-many indices
-- correspondences 
parsePh :: String -> [[(Int,[Int])]]
parsePh s = map (oneToMany . map toIdxPair . words) (lines s)
  where 
    toIdxPair :: String -> (Int,[Int])
    toIdxPair p = (read i :: Int, [read j :: Int])
      where [i,j] = splitOn "-" p
    oneToMany = M.toList . M.fromListWith (++)

-- | Parse a bitext in fast_align format to a list of pairs of sentences
parseBi :: String -> [(String, String)]
parseBi s = [(head s',last s') | s' <- map (splitOn " ||| ") (lines s)]

-- | Given a list of (target, language) sentences and a list of alignment
-- indices, return the corresponding pairs of (multi)words as 
-- a set of Alignments
phFileToAlignments :: [(String,String)] -> [[(Int,[Int])]] -> AlignMap
phFileToAlignments _ [] = M.empty -- length is supposed to be the same though
phFileToAlignments [] _ = M.empty
phFileToAlignments (s:ss) (ps:pss) = 
    phSentToAlignment s ps `union'` phFileToAlignments ss pss
  where
    phSentToAlignment :: (String,String) -> [(Int,[Int])] -> AlignMap
    phSentToAlignment _ [] = M.empty
    phSentToAlignment (src,trg) ((i,js):ps) = 
        a `insert'` phSentToAlignment (src,trg) ps
      where 
        a = (AT (t,u),initMeta { reasons = S.singleton KNOWN })
        (t,u) = (mockUDTree one,mockUDTree many) 
        mockUDTree x = 
            RTree ((initUDWord 1) { udFORM = filter (/= '\n') x }) []
        one = words src !! i
        many = unlines $ reverse $ intersperse " " [words trg !! j | j <- js]

-- | CoNLL-U aligned strings to fast-align bitext conversion (useful to feed 
--   fast_align the same tokens)
conllu2bi :: (String,String) -> String
conllu2bi (c1,c2) = unlines $ zipWith (\s1 s2 -> s1 ++ " ||| " ++ s2) ts1 ts2
  where
    (ss1,ss2) = mapPair parseUDText (c1,c2) 
    (ts1,ts2) = mapPair (map (unwords . getTks . udWordLines)) (ss1,ss2)
      where
        getTks [] = []
        getTks (x:xs) = udFORM x:getTks xs'
          where xs' = case udID x of
                  (UDIdRange n m) -> drop (m - n + 1) xs
                  _ -> xs
    mapPair f (x,y) = (f x, f y)
