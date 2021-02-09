align :: (UDTree,UDTree) -> Alignment
align (RTree n ts,RTree m us) = (RTree n pts, RTree m pus)
   where 
      (pts,pus) = unzip [align (t,u) | 
      (t,u) <- padSubtrees (sortSubtrees ts) (sortSubtrees us)]
         where
            root (RTree n _) = n
            sortSubtrees = sortOn (udDEPREL . root)
            padSubtrees xs ys = case (xs,ys) of
               (x:xs', y:ys')
                  | k x == k y -> (x,y):padSubtrees xs' ys'
                  | k x <  k y -> (x,d):padSubtrees xs' ys
                  | k x >  k y -> (d,y):padSubtrees xs  ys'
               (_,[]) -> [(x,d) | x <- xs]
               ([],_) -> [(d,y) | y <- ys]
               _ -> []
               where
                  d = padUDNode -- dummy node used for padding
                  -- padding key to compare subtrees
                  k t = (udDEPREL (root t), 
                        distanceDepNode (root t))