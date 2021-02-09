module PreprocessUtils where
    import System.Environment (getArgs)
    import Data.List
    import Data.List.Split


    -- Usage: runhaskell tagged_file
    main = do
        args <- getArgs
        case args of
            [f] -> do
                content <- readFile f
                let ss = map (map fields . lines) (sentences content)
                let ss' = map (map wordform . denumber) ss
                mapM_ (putStrLn . unlines) ss'
            _ -> putStrLn "Usage: runhaskell tagged_file"
            where
                sentences = splitOn "\n\n"
                fields = splitOn "\t"
                wordform = (!! 0)
                denumber = dropWhile (\fs -> (fs !! 2) `elem` numberingTags)
                    where numberingTags = ["Z", "Fz", "Fpa", "Fpt"]