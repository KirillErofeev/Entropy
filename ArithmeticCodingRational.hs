{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\))
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (toList, empty, alter, fromList)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)

import Data.Numbers.Primes as Primes

--import Entropy (chrFreqs)

fileWords = "data/words.txt"

type ProbabilityModel = Map Char Rational
type Intervals        = Map Char (Rational, Rational)
type Interval         = (Rational, Rational)


chrFreqs :: String -> Map Char Integer
chrFreqs = foldl' chrToFreqs (Map.empty) where
    chrToFreqs m chr = Map.alter add chr m
    add Nothing  = Just   1
    add (Just x) = Just $ 1 + x

charProbs :: String -> ProbabilityModel 
charProbs text = fmap (% (fromIntegral $ length text)) $ chrFreqs text

charProbsToProbModel :: ProbabilityModel -> Intervals
charProbsToProbModel cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

testString = "Sai"

arithmeticCoding :: Intervals -> String -> Interval
arithmeticCoding invls = foldl' (flip encode) (0%1, 1%1) . map (\c -> invls ! c) where
    dif (codeL, codeR)              = codeR - codeL                         
    encode (l,r) p@(codeL, codeR)   = (dif p * l + codeL, dif p * r + codeL)
    getCodeNumber p@(codeL, codeR)  = codeL + dif p * (1%2)                 

--codeToBytes :: Interval -> ByteString
--codeToBytes 

{-- View --}
mapToSortBySndList :: ProbabilityModel -> [(Char, Rational)]
mapToSortBySndList = sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList

showColumnList :: Show a => [a] -> IO ()
showColumnList = sequence_ . map (\x -> putStrLn (show x))
{-- View (end) --}

runRatAC = do
    text <- (fmap . fmap) toLower (readFile fileWords)
    let text' = take 24 text
    let probs = charProbs text'
    let intervals  = charProbsToProbModel probs
    let inl        = arithmeticCoding intervals text'
    print text'
    showColumnList $ mapToSortBySndList probs
    showColumnList $ Map.toList intervals
    print $ arithmeticCoding intervals text'
    return () 
