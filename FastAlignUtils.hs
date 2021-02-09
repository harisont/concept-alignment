module FastAlignUtils where

import System.Environment (getArgs)
import Data.List
import Data.List.Split
import qualified Data.Set as S
import qualified Data.Map as M
import UDConcepts
import ConceptAlignment

-- parse a string in pharaoh format to a list of one-to-many indices
-- correspondences 
parsePh :: String -> [[(Int,[Int])]]
parsePh s = map (oneToMany . map toIdxPair . words) (lines s)
    where 
        toIdxPair :: String -> (Int,[Int])
        toIdxPair p = (read i :: Int, [read j :: Int])
            where [i,j] = splitOn "-" p
        oneToMany = M.toList . M.fromListWith (++)

-- parse a bitext in fast_align format to a list of pairs of sentences
parseBi :: String -> [(String, String)]
parseBi s = [(head s',last s') | s' <- map (splitOn " ||| ") (lines s)]

-- given a list of (target, language) sentences and a list of alignment
-- indices, return the corresponding pairs of (multi)words as 
-- LinAlignments
phToLas :: [(String,String)] -> [[(Int,[Int])]] -> [LinAlignment]
phToLas ss is = 
    map 
        (\(lts,(r,o)) -> LAlignment lts (S.toList r) o) 
        (M.toList . M.fromListWith combineVals $ phToLas' ss is)
  where 
    phToLas' _ [] = [] -- length is supposed to be the same though
    phToLas' [] _ = []
    phToLas' (s:ss) (ps:pss) = phToLa s ps ++ phToLas' ss pss
        where
            phToLa _ [] = []
            phToLa (s,t) ((i,js):ps) = 
                (lt,(S.singleton FAST,1)):phToLa (s,t) ps
                where 
                    lt = (filter (/= '\n') one,filter (/= '\n') many) 
                    one = words s !! i
                    many = unlines $ reverse $ 
                           intersperse " " [words t !! j | j <- js]

-- | CoNLL-U aligned strings to fast-align bitext conversion (useful to feed 
--   fast_aling the same tokens)
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
