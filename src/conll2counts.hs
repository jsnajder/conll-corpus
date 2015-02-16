-- conll2counts.hs
-- (c) 2015 Jan Snajder
--
-- Word counts mapper/reducer for Hadoop streams for CONLL parsed file
-- Counts word forms
--   -w  wordforms
--   -l  lemmas
--   -L  lemmas, with fallback to wordforms in case of '<unknown>'
--   -p  concatenates pos tags
--
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import ConllReader
import qualified Data.Counts as C
import Data.Either
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ParseArgs
import System.Environment
import System.IO

type WordCounts = C.Counts Text

count :: (Token -> [String]) -> String -> WordCounts
count f = 
  C.fromList . concatMap (map T.pack . f) . mapMaybe parseLine . lines

mrMap :: (Token -> [String]) -> String -> String
mrMap f = unlines . concatMap f . mapMaybe parseLine . lines

mrReduce :: Text -> Text
mrReduce = showCounts . C.fromList . T.lines

showCounts :: WordCounts -> Text
showCounts = 
  T.unlines . map (\(w,c) -> T.concat [w,"\t",T.pack $ show c]) . C.counts

arg = 
  [ Arg 0 (Just 'm') (Just "map") Nothing 
      "run as hadoop mapper (reads from stdin)"
  , Arg 1 (Just 'r') (Just "reduce") Nothing 
      "run as hadoop reducer (reads from stdin)"
  , Arg 2 (Just 'w') (Just "wordforms") Nothing 
      "count wordforms"
  , Arg 3 (Just 'l') (Just "lemmas") Nothing 
      "count lemmas (default)"
  , Arg 4 (Just 'L') (Just "lemmas-backoff") Nothing 
      "count lemmas with backoff to wordforms for <unknown> lemmas"
  , Arg 5 (Just 'p') (Just "pos") Nothing 
      "append coarse-grained part-of-speech tag (CPOSTAG) to wordform/lemma"
  , Arg 6 Nothing Nothing  (argDataOptional "filename" ArgtypeString)
      "corpus in CoNLL format" ]

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete arg
  let wordform     = gotArg args 2
      lemmabackoff = gotArg args 4
      pos          = gotArg args 5
      f            = getArg args 6
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  let g = case (wordform,lemmabackoff,pos) of
          (True, _   , True)  -> (:[]) . formPos
          (True, _   , False) -> (:[]) . form
          (_   , True, True)  -> lemmaPos'
          (_   , True, False) -> lemma'
          (_   , _   , True)  -> lemmaPos
          _                   -> lemma
  if gotArg args 0 then interact $ mrMap g
  else if gotArg args 1 then T.interact mrReduce
  else if gotArg args 6 then
    readFile (fromJust f) >>= T.putStr . showCounts . count g
  else usageError args "Missing input file."
  hFlush stdout

