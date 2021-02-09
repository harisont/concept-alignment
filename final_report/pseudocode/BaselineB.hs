extract :: Alignment -> [Alignment]
extract (t,u) = zip (subts' t) (subts' u) 
    where
        subts' t = let ts = subts t in ts ++ map headt ts
            where 
                subts t = t:concatMap subts (immediateSubts t) 
                    where immediateSubts (RTree _ ts) = ts
                headt (RTree n _) = RTree n []