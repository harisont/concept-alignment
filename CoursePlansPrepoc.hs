module CoursePlansPreproc where
    import System.Environment (getArgs)
    import Data.List.Split

    main = do
        args <- getArgs
        file' <- case args of
            [file] -> do
                readFile file >>= return . map unwhitespace . concatMap sentences . lines
            _ -> do 
                putStrLn "Usage: runghc CoursePlansPreproc file"
                return []
        putStrLn $ unlines file'

    sentences :: String -> [String]
    sentences = splitOn ". "

    unwhitespace :: String -> String
    unwhitespace = dropWhile isWhitespace
        where isWhitespace c = c == ' ' || c == '\t'