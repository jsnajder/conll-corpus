-- (c) 2013 Jan Snajder
--
-- Word counts mapper/reducer for Hadoop streams for CONLL parsed file
-- Counts word forms
--   -w  wordforms
--   -l  lemmas
--   -L  lemmas, with fallback to wordforms in case of '<unknown>'
--   -p  concatenates pos tags

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import ConllReader
import qualified Data.Counts as C
import Data.Either
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ParseArgs
import System.Environment
import System.IO

type WordCounts = C.Counts Text

count :: (Token -> [String]) -> String -> WordCounts
count f = C.fromList . concatMap (map T.pack . f) . rights . map parseLine . lines

mrMap :: (Token -> [String]) -> String -> String
mrMap f = unlines . concatMap f . rights . map parseLine . lines

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
  , Arg 2 (Just 'w') (Just "--wordforms") Nothing 
      "count wordforms"
  , Arg 3 (Just 'l') (Just "--lemmas") Nothing 
      "count lemmas (default)"
  , Arg 4 (Just 'L') (Just "--lemmas-backoff") Nothing 
      "count lemmas with backoff to wordforms for <unknown> lemmas"
  , Arg 5 (Just 'p') (Just "--pos") Nothing 
      "append coarse-grained part-of-speech (CPOSTAG) to wordform/lemma"
  , Arg 6 (Just 'P') (Just "--pos-first") Nothing 
      "same as --pos, but takes only the first letter of CPOSTAG"
  , Arg 7 Nothing Nothing  (argDataOptional "filename" ArgtypeString)
      "CoNLL file" ]

data Pos = None | All | First

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete arg
  let wordform     = gotArg args 2
      lemmabackoff = gotArg args 4
      pos          = if gotArg args 5 then All else 
                       if gotArg args 6 then First else None
      f            = getArg args 7
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  let g = case (wordform,lemmabackoff,pos) of
          (True, _   , All)   -> (:[]) . formPos
          (True, _   , First) -> (:[]) . formPos'
          (True, _   , None ) -> (:[]) . form
          (_   , True, All)   -> lemmaFormPos
          (_   , True, First) -> lemmaFormPos'
          (_   , True, None)  -> lemmaForm
          (_   , _   , All )  -> lemmaPos
          (_   , _   , First) -> lemmaPos'
          _                   -> lemma
  if gotArg args 0 then interact $ mrMap g
  else if gotArg args 1 then T.interact mrReduce
  else if gotArg args 7 then
    readFile (fromJust f) >>= T.putStr . showCounts . count g
  else usageError args "Missing input file."
  hFlush stdout

