{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\))
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (toList, empty, alter, fromList)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)
import Data.Word8 (Word8)

import Debug.Trace 

import Data.Numbers.Primes as Primes

import Entropy (chrFreqs)

fileWords = "data/words.txt"

{--Intervals--}
type ProbabilityModel = Map Char Double
type Intervals        = Map Char (Double, Double)
type Interval         = (Double, Double)

belong :: Double -> Interval -> Bool
belong v (l,r) = v > l && v <= r

isSup v (_,r) = v > r
isInf v (l,_) = v <= l

dif (l, r) = r - l

mapInvls (wl, wr) (chl, chr) = ((wr - wl) * chl + wl, (wr - wl) * chr + wl)

binCenter (l, r) = (0.5 - l) / (r - l)

mapBoth fun (f,s) = (fun f, fun s)

updateWorkInvl1 = mapBoth (\x -> (x - 0.5)*2)
updateWorkInvl0 = mapBoth (*2)

{--Get probability model--}
charProbs :: String -> ProbabilityModel 
charProbs text = fmap (/ (fromIntegral $ length text)) $ chrFreqs text

charProbsToProbModel :: ProbabilityModel -> Intervals
charProbsToProbModel cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

{--Encoding--}
arithmeticCoding :: Intervals -> String -> [Word8]
arithmeticCoding invls = reverse . (1:) . fst . foldl encode ([],(0.0,1.0)) . map (invls !) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvls wInvl chInvl)

{-- View --}
mapToSortBySndList :: ProbabilityModel -> [(Char, Double)]
mapToSortBySndList = sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList

showColumnList :: Show a => [a] -> IO ()
showColumnList = sequence_ . map (\x -> putStrLn (show x))

{--Run Arithmetic Coding--}
runAC = do
    text <- (readFile fileWords)
    print text

runAC' = do
    text <- (fmap . fmap) toLower (readFile fileWords)
    text <- return "aabba"
    let text' = take 17 text
    let probs = charProbs text'
    let intervals  = charProbsToProbModel probs
    print $ length text
    print text'
    showColumnList $ mapToSortBySndList probs
    showColumnList $ Map.toList intervals
    ac <- return $ arithmeticCoding intervals text'
    print $ ac
    --print $ arithmeticCoding intervals text'
    return () 
