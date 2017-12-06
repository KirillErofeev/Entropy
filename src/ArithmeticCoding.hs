{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\), unfoldr, find)
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (toList, empty, alter, fromList, map, elems)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)
import Data.Word8 (Word8)

import Debug.Trace 

import Data.Numbers.Primes as Primes

import Entropy (chrFreqs, entropy, fullCondEntropy, condProbs)

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

mapInvl (wl, wr) (chl, chr) = ((wr - wl) * chl + wl, (wr - wl) * chr + wl)

binCenter (l, r) = (0.5 - l) / (r - l)

mapBoth fun (f,s) = (fun f, fun s)

norm p = mapBoth (/ dif p) p

updateWorkInvl1 = mapBoth (\x -> (x - 0.5)*2)
updateWorkInvl0 = mapBoth (*2)

{--Get probability model--}
charProbs :: String -> ProbabilityModel 
charProbs text = fmap (/ (fromIntegral $ length text)) $ chrFreqs text

charProbsToProbModel :: ProbabilityModel -> Intervals
charProbsToProbModel cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

textEntropy :: String -> Double
textEntropy = (entropy . Map.elems . charProbs)

{--Encoding--}
arithmeticCoding :: Intervals -> String -> [Word8]
arithmeticCoding invls = reverse . (1:) . fst . foldl encode ([],(0.0,1.0)) . map (invls !) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = traceShow ("q", 0:bs, updateWorkInvl0 i) $ writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = traceShow ("w", 1:bs, updateWorkInvl1 i) $ writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = traceShow ("e", bs, i) $ (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvl wInvl chInvl)

{-- View --}
mapToSortBySndList :: ProbabilityModel -> [(Char, Double)]
mapToSortBySndList = sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList

showColumnList :: Show a => [a] -> IO ()
showColumnList = sequence_ . map (\x -> putStrLn (show x))

{--Test--}
getDoubleByBytes :: [Word8] -> Double
getDoubleByBytes bs = foldr find 0.0 $ zip bs [1..] where
    find (0, _) v = v
    find (1, n) v = v + 2.0**(-n)

decodingTest :: Intervals -> Double -> String
decodingTest invls v = unfoldr decode invls where
    decode workInvls = Just $ let
                                 (char, invl) = (fromJust . find (\(_, i) -> v `belong` i) . Map.toList) workInvls
                              in
                                 --traceShow (char, Map.map (mapInvl invl) invls) (char, Map.map (mapInvl invl) invls)
                                 (char, Map.map (mapInvl invl) invls)


{--Run Arithmetic Coding--}
runAC = do
    text <- (readFile fileWords)
    print text

runShortAC = do
    text <- return "cabwe"
    print text
    let probs = charProbs text
    let intervals  = charProbsToProbModel probs
    let len = length text
    putStrLn $ "Text length " ++ show len
    putStrLn $ "Entropy " ++ show (textEntropy text)
    putStrLn $ "Full Conditional Entropy " ++ show (fullCondEntropy text)
    showColumnList $ mapToSortBySndList probs
    showColumnList $ Map.toList intervals
    let ac = arithmeticCoding intervals text
    print ac
    let lengthCode = length ac
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let value = getDoubleByBytes ac
    print value
    print $ take len $ decodingTest intervals value
    return ()

runAC' = do
    _text <- (fmap . fmap) toLower (readFile fileWords)
    let text = take 5 _text
    let text = "dacbe"
    print text
    let probs = charProbs text
    let intervals = charProbsToProbModel probs
    putStrLn $ show intervals
    let len = length text
    putStrLn $ "Text length " ++ show len
    putStrLn $ "Entropy " ++ show (textEntropy text)
    putStrLn $ "Full Conditional Entropy " ++ show (fullCondEntropy text)
    --showColumnList $ mapToSortBySndList probs
    --showColumnList $ Map.toList intervals
    let ac = arithmeticCoding intervals text
    print ac
    let lengthCode = length ac
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let value = getDoubleByBytes ac
    print value
    print $ take len $ decodingTest intervals value
    return ()
