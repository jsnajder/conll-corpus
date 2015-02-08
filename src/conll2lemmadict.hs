-- conll2lemmadict.hs
-- (c) 2015 Jan Snajder
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import ConllReader
import Control.Applicative
import Control.Monad
import Data.Char (isLower)
import qualified Data.Counts as C
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ParseArgs
import System.Environment
import System.IO

readWordList :: FilePath -> IO (Set Text)
readWordList f = 
  S.fromList . map (head . T.words) . T.lines <$> T.readFile f

mapping :: Set Text -> Text -> [(Text,Text)]
mapping ws s = do
  t <- rights . map (parseLine . T.unpack) $ T.lines s
  guard $ if S.null ws then True 
    else T.pack (form t) `S.member` ws || 
         T.pack (form t ++ posSep ++ cpostag t) `S.member` ws
  l <- lemma t
  let lp = l ++ posSep ++ cpostag t
      wp = form t ++ posSep ++ cpostag t
  return (T.pack wp, T.pack lp)

mkDict :: [(Text,Text)] -> Map Text Text
mkDict xs = fmap (fst . maximumBy (comparing snd) . C.counts) d
  where d = foldl' (\d (w,l) -> ins w l d) M.empty xs
        ins w l d = M.insertWith (C.union) w (C.fromList [l]) d

mrMap :: Set Text -> Text -> Text
mrMap ws = T.unlines . map (\(w,l) -> T.concat [w,"\t",l]) . mapping ws

mrReduce :: Text -> Text
mrReduce = 
  T.unlines . map (\(w,l) -> T.concat [w,"\t",l]) . M.toList . 
  mkDict . map parse . T.lines
  where parse s = case T.splitOn "\t" s of
                    (w:l:_) -> (w,l)
                    _       -> error "no parse"

readInput :: Maybe FilePath -> IO Text
readInput f = do
  h <- case f of 
    Nothing -> return stdin
    Just f  -> openFile f ReadMode
  hSetEncoding h utf8
  T.hGetContents h

arg = 
  [ Arg 0 (Just 'm') (Just "map") Nothing 
      "run as hadoop mapper (reads from stdin)"
  , Arg 1 (Just 'r') (Just "reduce") Nothing 
      "run as hadoop reducer (reads from stdin)"
  , Arg 2 (Just 't') (Just "targets") 
      (argDataOptional "corpus" ArgtypeString)
      "optional targets list"
  , Arg 3 Nothing Nothing  (argDataOptional "filename" ArgtypeString)
      "corpus in CoNLL format" ]

main = do
  args <- parseArgsIO ArgsComplete arg
  hSetEncoding stdin utf8
  hSetEncoding stdout utf8
  ws <- case getArg args 2 of
          Just f  -> readWordList f
          Nothing -> return S.empty
  if gotArg args 0 then
    T.interact $ mrMap ws
  else if gotArg args 1 then
    T.interact $ mrReduce
  else if gotArg args 3 then do
    d <- mkDict . mapping ws <$> readInput (getArg args 3)
    T.putStr . T.unlines . map (\(w,l) -> T.concat [w,"\t",l]) $ M.toList d
  else usageError args "Missing input file."
  hFlush stdout

