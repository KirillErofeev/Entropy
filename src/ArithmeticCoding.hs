{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\))
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (toList, empty, alter)
import Data.Ratio ((%))

import Data.Numbers.Primes as Primes

--import Entropy (chrFreqs)

fileWords = "data/words.txt"

type ProbabilityModel = Map Char Rational
type Intervals        = Map Char (Rational, Rational)


chrFreqs :: String -> Map Char Integer
chrFreqs = foldl' chrToFreqs (Map.empty) where
    chrToFreqs m chr = Map.alter add chr m
    add Nothing  = Just   1
    add (Just x) = Just $ 1 + x

charProbs :: String -> ProbabilityModel 
charProbs text = fmap (% (fromIntegral $ length text)) $ chrFreqs text

charProbsToProbModel :: ProbabilityModel -> Intervals
charProbsToProbModel cp = foldrWithKey toInvls cp Map.empty where
    toInvls key pr invls | null invls = Map.alter add 

testString = "Sai"

arithmeticCoding :: String -> ProbabilityModel -> ByteString
arithmeticCoding text pm = map (\c -> fromJust $ pm ! c) text where


{-- View --}
mapToSortBySndList :: ProbabilityModel -> [(Char, Rational)]
mapToSortBySndList = sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList

showColumnList :: Show a => [a] -> IO ()
showColumnList = sequence_ . map (\x -> putStrLn (show x))
{-- View (end) --}

runAC = do
    text <- (readFile fileWords)
    print text

runAC' = do
    text <- (fmap . fmap) toLower (readFile fileWords)
    let probs  =  charProbs text
    showColumnList $ mapToSortBySndList probs
    return () 
