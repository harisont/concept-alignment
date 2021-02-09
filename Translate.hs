module Translate where
  
import PGF
import System.Environment (getArgs)
  
main :: IO ()
main = do
    file:_ <- getArgs
    gr <- readPGF file
    interact (translate gr)

translate :: PGF -> String -> String
translate gr s = unlines $ map (translateSent gr) (lines s)

translateSent :: PGF -> String -> String
-- try to parse in all languages
translateSent gr s = case parseAllLang gr (startCat gr) s of
    -- print all translations (exclude lg, the source language)
    (lg,ts):_ -> head $ 
        filter (notElem '[') [linearize gr l t | l <- languages gr, l /= lg, t <- ts]
    _ -> "no parse"