{-------------------------------------------------------------------------------

 CoNLL-parsed file reader. 
 CoNLL format is defined here: http://ilk.uvt.nl/conll/

 (c) 2013 Jan Snajder

-- TODO: Convert String -> Text

-------------------------------------------------------------------------------}

module ConllReader where

import Control.Applicative
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
import Data.Maybe
import System.IO

unk    = "<unknown>"
posSep = "_"

data Token = Token 
  { ix       :: Int
  , form     :: String
  , lemma    :: [String]
  , cpostag  :: String
  , postag   :: String
  , feats    :: [String]
  , dephead  :: Int
  , deprel   :: String 
  , pdephead :: Maybe Int
  , pdeprel  :: Maybe String 
  } deriving (Show,Ord,Eq)

type Sentence = M.Map Int Token
type Corpus   = [Sentence]
type Lemma    = String
type Pos      = String
type LemmaPos = (Lemma,Pos)
type DepRel   = String

mkSentence :: [Token] -> Sentence
mkSentence ts = M.fromList [(ix t,t) | t <- ts]

parseLine :: String -> Maybe Token
parseLine = parseLine' . splitOn "\t"

parseLine' :: [String] -> Maybe Token
parseLine' (s1:s2:s3:s4:s5:s6:s7:s8:s9:s10:_)
  | isJust ix && all isDigit s7 && (na s9 || all isDigit s9) = 
      Just $ Token 
      { ix       = fromJust ix
      , form     = s2
      , lemma    = if s3==unk then [] else splitOn "|" s3
      , cpostag  = s4
      , postag   = s5
      , feats    = if na s6 then [] else splitOn "|" s6
      , dephead  = read s7
      , deprel   = s8 
      , pdephead = if na s9  then Nothing else Just $ read s9
      , pdeprel  = if na s10 then Nothing else Just s10 }
  | otherwise   = Nothing
  where ix = readIx s1
        na = (`elem` ["_","-"])
parseLine' _ = Nothing

readIx :: String -> Maybe Int
readIx = (fst <$>) . listToMaybe . reads . reverse . takeWhile isDigit . reverse

-- skips sentences that contain unparsable lines
readCorpusStr :: String -> Corpus
readCorpusStr = 
  mapMaybe (fmap mkSentence . sequence . map parseLine) . 
  split (dropInitBlank . dropFinalBlank . dropDelims . condense $ whenElt emptyLine) . 
  lines
  where emptyLine = null . words

readCorpus :: FilePath -> IO Corpus
readCorpus f = readCorpusStr <$> readFile f

showCorpus :: Corpus -> String
showCorpus = unlines . map (unlines . map showToken . sentenceTokens)
  where showToken t = intercalate "\t" $ map (\f -> f t) 
          [show . ix, form, showLemma . lemma, cpostag,
           postag, const "_", show . dephead, deprel,
           fromMaybe "_" . fmap show . pdephead, fromMaybe "_" . pdeprel]
        showLemma l | null l    = unk
                    | otherwise = intercalate "|" l

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

