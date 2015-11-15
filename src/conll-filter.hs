
import ConllReader
import Control.Monad
import Data.List.Split (splitOn)
import Data.Maybe
import System.Console.ParseArgs
import System.Exit
import System.IO
import Text.Printf

arg = 
  [ Arg 0 Nothing Nothing  (argDataRequired "corpus" ArgtypeString)
      "corpus in CoNLL format" ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete arg
  case getArg args 0 of
    Nothing -> do
      usageError args "Missing input file."
      exitFailure
    Just f -> readCorpus f >>= putStr . showCorpus

