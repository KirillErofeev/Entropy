{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\), unfoldr, find, nub)
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (union, filterWithKey, size, toList, empty, alter, fromList, map, elems)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)
import Data.Word8 (Word8)
import Data.Bits (shift, (.|.))
import qualified Data.ByteString.Lazy as BS (pack, writeFile) 

import Data.Numbers.Primes as Primes

import Types 
import Entropy (elemProbs', jointProbs, toPairs, chrFreqs, entropy, fullCondEntropy, condProbs)

import Stat (squezze)

--updateWorkInvl1 = mapBoth (\x -> (x - 0.5)*2)
--updateWorkInvl0 = mapBoth (*2)

{--Get probability model--}
--charProbs :: String -> ProbabilityModel 
charProbs text = fmap (/ (lengthText)) $ chrFreqs text where
    lengthText = fromIntegral $ length text

probsToIntervals cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

charProbsToIntervals = probsToIntervals

splitCondProbsByChar chars condProbs = map getMap (nub chars) where
    filt char (c :| _) _ | char == c = True
                         | otherwise = False
    getMap char = Map.filterWithKey (filt char) condProbs

--condProbsToProbModel = undefined
condProbsToProbModel :: String -> CondProbabilityModel -> CondIntervals
condProbsToProbModel text = foldr Map.union Map.empty . map probsToIntervals . splitCondProbsByChar text 

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
                                                    invls    = probsToIntervals probs
                                                    charInvl = invls ! char
                                                    newAM    = updateAdaptMap adaptMap char
                                                in
                                                    (writeBits (bs, mapInvl wInvl charInvl), newAM)

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
runShortAC = do
    let text = "dacbe"
    print text
    let probs = charProbs text
    let intervals = charProbsToIntervals probs
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

runAC' text = do
    let probs = charProbs text
    let pairPrb = pairProbs text
    --showColumnList $ Map.toList probs
    let intervals = charProbsToIntervals probs
    let len = length text
    {--Information--}
    putStrLn $ "Text length " ++ show len
    putStrLn $ "Entropy " ++ show (textEntropy (text :: String))
    putStrLn $ "Full Conditional Entropy " ++ show (fullCondEntropy text)
    --showColumnList $ mapToSortBySndList probs
    --showColumnList $ Map.toList intervals
    {--Arithmetic Coding--}
    putStrLn $ "\nArithmetic Coding:"
    let ac = arithmeticCoding intervals text
    let lengthCode = length ac
    --putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let sqAc = squezze ac
    putStrLn $ "Size file in bytes " ++ (show $ length sqAc)
    BS.writeFile "src/HaskellAC" $ BS.pack sqAc
    putStrLn $ "Write in HaskellAC"
    {--Pair AC--}
    putStrLn $ "\nPair AC:"
    putStrLn $ "Pair Entropy " ++ show (textEntropy $ toDisPairs text )
    putStrLn $ "Pair Entropy / Symbol " ++ show ((/2) $ textEntropy $ toDisPairs text)
    let pi = probsToIntervals pairPrb
    let ac = pairAC pi text
    let lengthCode = length ac
    putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let sqAc = squezze ac
    putStrLn $ "Size file in bytes " ++ (show $ length sqAc)
    BS.writeFile "data/PairAC" $ BS.pack sqAc
    putStrLn $ "Write in PairAC"
    {--Condition AC--}
    putStrLn $ "\nCondition AC:"
    let condPrb = condProbs ('\n' : text)
    let intervals = condProbsToProbModel text condPrb
    let ac = condAC intervals text
    let lengthCode = length ac
    putStrLn $ "Size file in bytes " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let sqAc = squezze ac
    putStrLn $ "Size in bits " ++ (show $ length sqAc)
    BS.writeFile "data/condAC" $ (BS.pack . squezze) ac
    putStrLn $ "Write in condAC"
    {--Adaptive AC--}
    putStrLn $ "\nAdaptive AC:"
    let ac = adaptAC text
    let lengthCode = length ac
    putStrLn $ "Size in bits " ++ (show $ lengthCode)
    putStrLn $ "Bits/Symbol " ++ (show $ fromIntegral lengthCode / fromIntegral len)
    let sqAc = squezze ac
    putStrLn $ "Size file in bytes " ++ (show $ length sqAc)
    BS.writeFile "data/adaptAC" $ (BS.pack . squezze) ac
    putStrLn $ "Write in adaptAC"
    return ()

condAC :: CondIntervals -> String -> [Word8]
condAC ci = (reverse . (1:) . fst        .
                            foldl' encode ([],(0.0,1.0)) .
                            map (ci !) . toPairs) where
    writeBits (bs, i@(l,r))
         | isSup 0.5 i = writeBits (0:bs, updateWorkInvl0 i)
         | isInf 0.5 i = writeBits (1:bs, updateWorkInvl1 i)
         | otherwise   = (bs, i)
    encode (bs, wInvl@(wl, wr)) chInvl@(chl, chr) = writeBits $ (bs, mapInvl wInvl chInvl)
