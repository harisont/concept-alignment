propagate :: [Criterion] -> ([UDTree],[UDTree]) -> UDTree -> 
    Maybe Alignment
propagate cs ([],_) _ = Nothing 
propagate cs (t:ts,u:us) c = 
    let as = sortAlignments (alignExtract cs (t,u)) 
    in case find (\(sl,_) -> sl == c) as of
        Nothing -> propagate cs (ts,us) c
        a -> a
    where 
        sortAlignments = sortOn (depthDiff . fst)
            where
                depthDiff t = abs (depth t - depth c)