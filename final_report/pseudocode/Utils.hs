module Utils where

import Data.List

-- UD words/nodes are represented as a record type whose 
-- fields mimic those of a CoNLL-U file 
data UDWord = UDWord {
  udID     :: Int,
  udFORM   :: String,
  udLEMMA  :: String, 
  udUPOS   :: String,
  udXPOS   :: String,
  udFEATS  :: [String],
  udHEAD   :: Int,
  udDEPREL :: String,
  udDEPS   :: String,
  udMISC   :: [String]
  } deriving Eq

-- a rose tree is a root node followed by a list of rose trees
data RTree n = RTree n [RTree n] deriving Eq

-- a UD tree is a rose tree whose nodes are UD words
type UDTree = RTree UDWord

-- an alignment is a pair of dependency trees
type Alignment = (UDTree,UDTree)

type Criterion = UDTree -> UDTree -> Bool

alignSent :: [Criterion] -> (UDTree,UDTree) -> [Alignment]
alignSent cs (t@(RTree n ts), u@(RTree m us)) = 
    if (not . null) applyingCriteria 
        then (t,u):concatMap (alignSent cs) [(t,u) | 
            t <- sortSubtrees ts, u <- sortSubtrees us]
        else []
        where
            applyingCriteria = filter (\c -> c t u) cs
            sortSubtrees = sortOn (udDEPREL . root)

root :: RTree a -> a
root (RTree n _) = n 

depth :: RTree a -> Int
depth = undefined 