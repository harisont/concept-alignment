type Criterion = UDTree -> UDTree -> Bool

alignExtract :: [Criterion] -> (UDTree,UDTree) -> [Alignment]
alignExtract cs (t@(RTree n ts), u@(RTree m us)) = 
    if (not . null) applyingCriteria 
        then (t,u):concatMap (alignExtract cs) [(t',u') | 
            t' <- sortSubtrees ts, u's <- sortSubtrees us]
        else []
        where
            applyingCriteria = filter (\c -> c t u) cs
            sortSubtrees = sortOn (udDEPREL . root)
