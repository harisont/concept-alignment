module Main where

import TreeConv
import GetConfig (getSentences)
import Data.List
import qualified Data.Map as M
import System.Environment (getArgs)

-- usage: runghc AlignTrees file1.conllu file2.conllu

main = do
  a:b:_ <- getArgs
  testAlignDepTrees a b

-- read conll format from two files, which contain parallel UD trees
testAlignDepTrees :: FilePath -> FilePath -> IO ()
testAlignDepTrees a b = do
  ts <- readFile a >>= return . map (conll2deptree . unlines) . getSentences . lines
  us <- readFile b >>= return . map (conll2deptree . unlines) . getSentences . lines
  let tus = take 100 $ zip ts us -- TODO: rm take & drop!
  -- aligned trees
  let atus  = map (uncurry alignDepTrees) tus
  -- subtree alignments found in each tree
  let past (t,u) = prAlignment 2 (prDepTreeString t, prDepTreeString u)
  let pastus = [--prDepTree (fst tu) ++ prDepTree (snd tu) 
                  -- ++ uncurry prAlignedDepTrees atu ++
                  unlines (map past (alignedDepSubtrees atu)) 
                | (tu,atu) <- zip tus atus]
  mapM_ putStrLn pastus

padDepNode   :: DepNode
isPadDepNode :: DepNode -> Bool
padDepNode     = initDepNode{label="pad",source=666,udlabel="pad"}
isPadDepNode n = label n == "pad"

{- not used
alignUDTrees  :: DepTree -> DepTree -> (DepTree,DepTree)
alignUDTrees  = albl 
  where
   albl    = alignTrees (label . root)   (T padDepNode [])
   audlbl  = alignTrees (udlabel . root) (T padDepNode [])
   aclslbl = alignTrees (udlabel . root) (T padDepNode [])
-}

-- align two dep trees by sorting and padding
alignDepTrees :: DepTree -> DepTree -> (DepTree,DepTree)
alignDepTrees = alignTrees (udlabel . root) (\t -> (udlabel (root t), distanceDepNode (root t))) (T padDepNode [])
---alignDepTrees = (alignTrees (udlabel . root) (T padDepNode []))

bestAlignment :: DepTree -> (DepTree,DepTree) -> (DepTree,DepTree)
-- disambiguation many-to-many alignments to result in one-to-one alignment
-- many-to-many alignments can not always be disambiguated. use default value
-- d to disambiguate if needed
-- based on number of non-padded nodes for each tree pair
bestAlignment d pts = pts  
{-  where 
   fertility  = (-) (length . allnodes . fst) (length . allnodes . snd)
   pls        = zip pts (map fertility pts) 
   spts       = sortBy (\ (_,m) (_,n) -> compare n m) pls 
-}

-- align two trees by sorting and padding, with padding key k, sorting key h, and default value d 
alignTrees :: (Ord b,Ord c) => (Tree a -> b) -> (Tree a -> c) -> Tree a -> Tree a -> Tree a -> (Tree a, Tree a)
alignTrees k h d (T n ts) (T m us) = (T n pts, T m pus)
  where
   (pts,pus) = unzip [alignTrees k h d t u | (_,(t,u)) <- padLists k d (sorts ts) (sorts us)]
   sorts     = sortOn h
---align two trees by sorting and padding, with padding key f and default value d 
---alignTrees :: (Show a, Ord b) => (Tree a -> b) -> Tree a -> Tree a -> Tree a -> (Tree a, Tree a)
---alignTrees f d (T n ts) (T m us) = (T n pts, T m pus)
---  where
---   (pts,pus) = unzip [alignTrees f d t u | (_,(t,u)) <- padLists d (appf ts) (appf us)] -- many to many alignments between trees
---   appf xs   = [(f x, x) | x <- xs]

-- auxiliary: pad two parallel lists with a default value d; sorting is presupposed 
padLists :: Ord a => (b -> a) -> b -> [b] -> [b] -> [(a,(b,b))]
padLists k d xs ys = pads xs ys
  where
    pads xx yy = case (xx,yy) of
      (x:xx2, y:yy2)
        | k x == k y -> (k x, (x,y)) : pads xx2 yy2
        | k x <  k y -> (k x, (x,d)) : pads xx2 yy
        | k x >  k y -> (k y, (d,y)) : pads xx  yy2
      (_, []) -> [(k x,(x,d)) | x <- xx]
      ([], _) -> [(k y,(d,y)) | y <- yy]
      _ -> []
{- buggy
padLists :: Ord a => b -> [(a,b)] -> [(a,b)] -> [(a,(b,b))]
padLists d xs ys = [(k, (x,y)) | k<-xys, x<-xlook k, y<-ylook k] -- [(k, pair) | k<-xys, pair<-zip (xlook k) (ylook k)] 
 where 
  xys     = nub $ sort $ map fst xs ++ map fst ys
  mxs     = M.fromListWith (++) . map (\(x,y) -> (x,[y])) $ xs
  mys     = M.fromListWith (++) . map (\(x,y) -> (x,[y])) $ ys
  xlook k = maybe [d] id $ M.lookup k mxs
  ylook k = maybe [d] id $ M.lookup k mys
-}

-- aligned subtrees; assumes perfect alignment; add a tree for each head word, too
alignedSubtrees :: (Tree a,Tree a) -> [(Tree a,Tree a)]
alignedSubtrees (t,u) = zip (subs t) (subs u) 
  where
   subs t = let ts = subtrees t in ts ++ [T n [] | T n (_:_) <- ts]

-- corollary: aligned trees with pad nodes eliminated
alignedDepSubtrees :: (DepTree,DepTree) -> [(DepTree,DepTree)]
alignedDepSubtrees tu = [(unPad t,unPad u) | (t,u) <- alignedSubtrees tu, notPad t, notPad u]
  where
    notPad t = not (isPadDepNode (root t))
    unPad (T n ts) = T n (filter notPad ts)

-- print aligned trees line by line, separated by |||
prAlignedDepTrees :: DepTree -> DepTree -> String
prAlignedDepTrees t u = unlines prints 
  where
    pt = lines $ prDepTree t
    pu = lines $ prDepTree u
    mx = maximum $ map length pt
    prints = [prAlignment mx xy | xy <- zip pt pu]

prDepTreeString :: DepTree -> String
prDepTreeString t = unwords [word n | n <- sortOn position (allnodes t)]
-- prDepTreeString t = unwords [word n | n <- sortOn position (allnodes t), (not . isPadDepNode) n]

prAlignment :: Int -> (String,String) -> String
prAlignment mx (x,y) = (dropWhile (== ' ') x) ++ "|" ++ (dropWhile (== ' ') y)


