{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\), unfoldr, find, nub)
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (size, toList, empty, alter, fromList, map, elems)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)
import Data.Word8 (Word8)
import Data.Bits (shift, (.|.))
import qualified Data.ByteString.Lazy as BS (pack, writeFile) 

import Debug.Trace 

import Data.Numbers.Primes as Primes

import Entropy (elemProbs', jointProbs, toPairs, chrFreqs, entropy, fullCondEntropy, condProbs, CharCond(..))

fileWords = "data/words.txt"

{--Intervals--}
type ProbabilityModel = Map Char Double
type Intervals        = Map Char Interval
type CondIntervals    = Map CharCond Interval
type Interval         = (Double, Double)

data AdaptMap k v = AdaptMap {getMap :: (Map k v), getSumWeight :: Double}

initAdaptMap :: String -> AdaptMap Char Double
initAdaptMap text = AdaptMap
            (Map.fromList $ zip (nubText) (repeat 1.0)) (fromIntegral $ length nubText) where
    nubText = nub text

updateAdaptMap (AdaptMap m w) char = AdaptMap (Map.alter up char m) (w+1.0) where
    up (Just frq) = Just $ frq + 1.0


normAdaptMap (AdaptMap m w) = Map.map (/w) m

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
--charProbs :: String -> ProbabilityModel 
charProbs text = fmap (/ (lengthText)) $ chrFreqs text where
    lengthText = fromIntegral $ length text

probsToProbModel cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

charProbsToProbModel = probsToProbModel

--textEntropy :: String -> Double
textEntropy = (entropy . Map.elems . charProbs)

{--Encoding--}
arithmeticCoding :: Intervals -> String -> [Word8]
arithmeticCoding invls = reverse . (1:) . fst . foldl' encode ([],(0.0,1.0)) . map (invls !) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvl wInvl chInvl)

pairProbs = elemProbs' . toDisPairs

pairAC :: CondIntervals -> String -> [Word8]
pairAC ci text = (reverse . (1:) . fst        .
                            foldl' encode ([],(0.0,1.0)) .
                            map (ci !) . toDisPairs) (text) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvl wInvl chInvl)

toDisPairs [] = []
toDisPairs [x] = [(x :| '\n')]
toDisPairs (x:y:xs) = (x :| y) : toDisPairs xs

fst3 (a,_,_) = a
make3 (a,b) c = (a,b,c)


adaptAC :: String -> [Word8]
adaptAC text = (reverse . (1:) . fst . fst . foldl' encode (([], (0.0,1.0)), init) ) text where
    init = initAdaptMap text
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode ((bs, wInvl@(wl, wr)), adaptMap) char = let
                                                    probs    = normAdaptMap adaptMap
                                                    invls    = probsToProbModel probs
                                                    charInvl = invls ! char
                                                    newAM    = updateAdaptMap adaptMap char
                                                in
                                                    (writeBits (bs, mapInvl wInvl charInvl), newAM)

{--Pack to binary--}
groupByN :: Int -> [a] -> [[a]]
groupByN n = foldr tot [] where
    tot x []                        = [[x]]
    tot x (xs:xss) | length xs < n  = (x:xs):xss
                   | otherwise      = [x]:xs:xss

addZerosToN :: Int -> [Word8] -> [Word8]
addZerosToN n xs | length xs < n = addZerosToN n (0:xs)
                 | otherwise     = xs

toBits :: [Word8] -> Word8
toBits bs | length bs <= 8 && (not . null) bs
                  = (foldl' toBit 0 . addZerosToN 8) bs where
    toBit byte bit = shift byte 1 .|. bit

squezze :: [Word8] -> [Word8]
squezze = (map toBits . groupByN 8)

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
    let text = "dacbe"
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
    let text = take 500001 _text
    let probs = charProbs text
    --print text
    --showColumnList $ Map.toList probs
    let intervals = charProbsToProbModel probs
    let len = length text
    {--Information--}
    putStrLn $ "Text length " ++ show len
    putStrLn $ "Entropy " ++ show (textEntropy text)
    --putStrLn $ "Full Conditional Entropy " ++ show (fullCondEntropy text)
    --showColumnList $ mapToSortBySndList probs
    --showColumnList $ Map.toList intervals
    {--Arithmetic Coding--}
    putStrLn $ "\nArithmetic Coding:"
    let ac = arithmeticCoding intervals text
    let lengthCode = length ac
    putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    BS.writeFile "data/AC" $ (BS.pack . squezze) ac
    {--Pair AC--}
    putStrLn $ "\nPair AC:"
    let pp = pairProbs text
    putStrLn $ "Pair Entropy " ++ show (textEntropy $ toDisPairs text)
    putStrLn $ "Pair Entropy / Symbol " ++ show ((/2) $ textEntropy $ toDisPairs text)
    let pi = probsToProbModel pp
    let ac = pairAC pi text
    let lengthCode = length ac
    putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    BS.writeFile "data/pairAC" $ (BS.pack . squezze) ac
    {--Adaptive AC--}
    putStrLn $ "\nAdaptive AC:"
    let ac = adaptAC text
    let lengthCode = length ac
    putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    BS.writeFile "data/adaptAC" $ (BS.pack . squezze) ac
    return ()

condAC :: CondIntervals -> String -> [Word8]
condAC ci text = (reverse . (1:) . fst        .
                            foldl' encode ([],(0.0,1.0)) .
                            map (ci !) . toPairs) ('\n' : text) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvl wInvl chInvl)

