-- conll2lemmadict.hs
-- (c) 2015 Jan Snajder
-- 
-------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

import ConllReader
import Control.Applicative
import Control.Monad
import Data.Char (isLower)
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
import Debug.Trace

readWordList :: FilePath -> IO (Set Text)
readWordList f = 
  S.fromList . map (head . T.words) . T.lines <$> T.readFile f

mapping :: Set Text -> String -> [(String,String)]
mapping ws s = do
  t <- rights . map parseLine $ lines s
  guard $ if S.null ws then True 
    else T.pack (form t) `S.member` ws || 
         T.pack (form t ++ posSep ++ cpostag t) `S.member` ws
  l <- lemma t
  let lp = l ++ posSep ++ cpostag t
      wp = form t ++ posSep ++ cpostag t
  return (wp, lp)

mkDict :: [(String,String)] -> Map Text Text
mkDict xs = fmap (fst . maximumBy (comparing snd) . M.toList) d
  where d = foldl' (\d (w,l) -> ins w l d) M.empty xs
        ins w l d = let w' = T.pack w
                        l' = T.pack l
                    in case M.lookup w' d of
                      Nothing -> M.insert w' (M.singleton l' 1) d
                      Just c  -> M.adjust (M.insertWith (+) l' 1) w' d

mrMap :: Set Text -> String -> String
mrMap ws = unlines . map (\(w,l) -> w ++ "\t" ++ l) . mapping ws

mrReduce :: String -> String
mrReduce = 
  unlines . map (\(w,l) -> T.unpack w ++ "\t" ++ T.unpack l) . M.toList . 
  mkDict . map parse . lines
  where parse s = case words s of
                    (w:l:_) -> (w,l)
                    _       -> error "no parse"

readInput :: Maybe FilePath -> IO String
readInput f = do
  h <- case f of 
    Nothing -> return stdin
    Just f  -> openFile f ReadMode
  hSetEncoding h utf8
  hGetContents h

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
    interact $ mrMap ws
  else if gotArg args 1 then
    interact $ mrReduce
  else if gotArg args 3 then do
    d <- mkDict . mapping ws <$> readInput (getArg args 3)
    putStr . unlines . map (\(w,l) -> T.unpack w ++ "\t" ++ T.unpack l) $ M.toList d
  else usageError args "Missing input file."
  hFlush stdout

