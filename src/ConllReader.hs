{-------------------------------------------------------------------------------

 CoNLL-parsed file reader. 
 CoNLL format is defined here: http://ilk.uvt.nl/conll/

 (c) 2013 Jan Snajder

-- TODO: Convert String -> Text

-------------------------------------------------------------------------------}

module ConllReader where

import Control.Applicative
import Data.Char
import Data.Either
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import System.IO

unk    = "<unknown>"
posSep = "_"

data Token = Token {
  ix      :: Int,
  form    :: String,
  lemma   :: [String],
  cpostag :: String,
  postag  :: String,
  feats   :: [String],
  dephead :: Int,
  deprel  :: String }
  deriving (Show,Ord,Eq)

type Sentence = M.Map Int Token
type Corpus   = [Sentence]
type Lemma    = String
type Pos      = String
type LemmaPos = (Lemma,Pos)
type DepRel   = String

mkSentence :: [Token] -> Sentence
mkSentence ts = M.fromList [(ix t,t) | t <- ts]

parseLine :: String -> Either String Token
parseLine = parseLine' . splitOn "\t"

parseLine' :: [String] -> Either String Token
parseLine' (s1:s2:s3:s4:s5:s6:s7:s8:_)
  | isJust ix && all isDigit s7 = Right $ Token 
      { ix      = fromJust ix
      , form    = s2
      , lemma   = if s3==unk then [] else splitOn "|" s3
      , cpostag = s4
      , postag  = s5
      , feats   = if s6=="_" then [] else splitOn "|" s6
      , dephead = read s7
      , deprel  = s8 }
  | otherwise = Left $ "Index and DepHead must be integers"
  where ix = readIx s1
parseLine' _ = Left $ "Cannot parse"

readIx :: String -> Maybe Int
readIx = (fst <$>) . listToMaybe . reads . dropWhile (not . isDigit)

readCorpusStr :: String -> Corpus
readCorpusStr = 
  map (mkSentence . rights . map parseLine) . splitOn [""] . lines

readCorpus :: FilePath -> IO Corpus
readCorpus f = readCorpusStr <$> readFile f

showCorpus :: Corpus -> String
showCorpus = unlines . map (unlines . map showToken . sentenceTokens)
  where showToken t = intercalate "\t" $ map (\f -> f t) 
          [show . ix, form, intercalate "|" . lemma, cpostag,
           postag, const "_", show . dephead, deprel] 

headToken :: Sentence -> Token -> Maybe Token
headToken s t = case dephead t of
              0 -> Nothing
              i -> M.lookup i s

sentenceTokens :: Sentence -> [Token]
sentenceTokens = M.elems

depTokens :: Sentence -> Token -> [Token]
depTokens s t = filter ((==ix t) . dephead) $ M.elems s

depTokensBy :: DepRel -> Sentence -> Token -> [Token]
depTokensBy r s = filter ((==r) . deprel) . depTokens s

adjTokens :: Sentence -> Token -> [Token]
adjTokens s t = catMaybes [headToken s t] ++ depTokens s t

getLP :: Token -> [LemmaPos]
getLP t = [(l,cpostag t) | l <- lemma t]

-- fallbacks to wordform in case of unknown lemma
getLP' :: Token -> [LemmaPos]
getLP' t = if unknownLemma t then [(form t,postag t)] else getLP t

showLP :: LemmaPos -> String
showLP (l,[]) = l
showLP (l,p)  = l ++ posSep ++ p

lemmaPos :: Token -> [String]
lemmaPos = map showLP . getLP

lemma' :: Token -> [String]
lemma' t = case lemma t of
  [] -> [form t]
  ls -> ls

lemmaPos' :: Token -> [String]
lemmaPos' = map showLP . getLP'

formPos :: Token -> String
formPos t = case (form t, cpostag t) of
  (w,[]) -> w
  (w,p)  -> w ++ posSep ++ p

unknownLemma :: Token -> Bool
unknownLemma = null . lemma

