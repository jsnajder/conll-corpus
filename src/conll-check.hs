
-- FIX this! Should accept lines with whitespaces as sentence delimiters

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
  if not $ gotArg args 0 then
    usageError args "Missing input file."
    exitFailure
  else do
    s <- readFile . fromJust $ getArg args 0
    let ss = splitOn [""] $ lines s
    forM_ (zip ss [1..]) $ \(s,i) -> do
      let xs = filter (isNothing . parseLine . fst) $ zip s [1..]
      forM xs $ \(l,j) ->
        putStrLn $ printf "sentence %d, token %d: cannot parse \"%s\"" (i::Int) (j::Int) l

