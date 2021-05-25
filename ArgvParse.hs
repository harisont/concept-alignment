module ArgvParse where

import System.Console.GetOpt
import System.Exit

-- | All flags used in the 5 executables. Which ones is used in which is
-- specified in each module's options constant
data Flag = Pharaoh String
          | Path String
          | MaxSize String
          | Pattern String
          | ExtractionGrammar String
          | MorphoDicts String
          | All
          | Clauses 
          | Rest
          | Linearize
          | Simple
          | Help
          | Reasons
  deriving (Show,Eq)

-- | Type synonym for command line args (non-options)
type Arg = String

-- | Given a list of command line args, a help message and a list of
-- option descriptions, return flags and args separately or quit the program
-- showing usage if the input is invalid
parseArgv :: [String] -> String -> [OptDescr Flag] -> IO ([Flag], [Arg])
parseArgv argv help options = 
  case getOpt Permute options argv of
    (flags,args,[]) -> return (flags,args)
    (_,_,errs) -> putStrLn (concat errs ++ help) >> exitWith (ExitFailure 1)