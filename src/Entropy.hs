{-# LANGUAGE NoMonomorphismRestriction #-}
module Entropy where

import qualified Data.Map as Map
import Data.Map (Map, keys, elems, mapWithKey, (!))
import qualified Data.Map as Map (map)
import Data.List
import qualified Data.Set as Set
import Data.Tuple (swap)

import Data.Numbers.Primes as Primes

import Types (CharCond(..))

primeStream :: [Int]
primeStream = 2 : [x | x <- [3..],
                           all (\y -> x `mod` y /= 0)          .
                           map round                           .
                           takeWhile (<= sqrt(fromIntegral x)) .
                           map fromIntegral                    $
                           primeStream]

twinPrimesStream = 3 : 5 : 7 : (twinFilter . dropWhile (<= 7)) primes where
     twinFilter (x:y:xs) | y - x == 2 = x : y : twinFilter xs
                         | otherwise  = twinFilter (y:xs)

fileName   = "data/primes10e5"
edgeOfPrimes   = 100000000

writePrimes = (writeFile fileName . init . tail . show . takeWhile (< edgeOfPrimes)) primes

writeTwinPrimes = (writeFile fileName . init . tail . show . takeWhile (< edgeOfPrimes)) twinPrimesStream

entropy :: [Double] -> Double
entropy = negate . sum . map (\p -> p * logBase 2 p)

rev (l :| r) = r :| l

toCharCond []  = []
toCharCond [x] = []
toCharCond (x:x0:xs) = (x :| x0) : toCharCond (x0:xs)

toPairs = toCharCond

normalizePrev chrFreqs (_ :| cc) frq = let
                                           Just a = Map.lookup cc chrFreqs
                                       in
                                           frq / a

normalizeNext chrFreqs (c :| _) frq = let
                                          Just a = Map.lookup c chrFreqs
                                      in
                                          frq / a

emptyMap allPairChars = Map.fromList [(cs :| csNext, 0) | cs <- allPairChars, csNext <- allPairChars]


chrCondProbs :: (CharCond -> Double -> Double) -> String -> Map CharCond Double
chrCondProbs normalize text = mapWithKey normalize $ foldl' ccToFreqs (emptyMap allPairChars) (toCharCond text) where
    allPairChars = Set.toAscList . Set.fromList $ text
    ccToFreqs m cc = Map.alter add cc m
    add (Just x) = Just $ 1.0 + x

chrCondPrev chrFreqs = chrCondProbs (normalizePrev chrFreqs)
chrCondNext chrFreqs = chrCondProbs (normalizeNext chrFreqs)

check = id 
--check = trace "1"

elemProbs :: Ord a => [a] -> Map a Double
elemProbs text = check $ Map.map (/ lengthText) $ chrFreqs text where
    lengthText = fromIntegral $ length text

elemProbs' text = Map.map (/ (lengthTextPlusOne)) $ chrFreqs text where
    lengthTextPlusOne = fromIntegral $ (length text + 1)

jointProbs = elemProbs' . toPairs

condProbs :: String -> Map CharCond Double
condProbs text = mapWithKey toCondProbs $ jointProbs text where
    probs = elemProbs text
    toCondProbs (c :| cc) jp = jp / probs ! c

fullCondEntropy :: String -> Double
fullCondEntropy text = (negate . sum . elems . mapWithKey f) $ condProbs text where
    jps = jointProbs text
    f (c :| cc) condProb = let
                              jp = jps ! (c :| cc)
                           in
                              if abs condProb > 1e-17
                                  then jp * logBase 2 condProb
                                  else 0

mapTo2D :: Map CharCond Double -> [[Double]]
mapTo2D = (map . map $ snd) . (foldr to2d []) . (Map.toList) where
    to2d elem []       = [[elem]]
    to2d elem (l@(x:xs):xss) | (leftCc . fst) elem  == (leftCc . fst) x = (elem : l) : xss
    to2d elem (l@(x:xs):xss) | otherwise                                = [elem] : l : xss

runEntropy = do
    text           <- (fmap $ filter (/= ',')) (readFile fileName)
    --text <- return "qe2"
    text <- return $ take 11111 text
    let charProbs  = elemProbs text
    let sortAndShow = (\f -> sequence_ . showColumn . fmap f . sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList)

    --print text

    putStrLn "Probabilities of chars"
    sortAndShow id charProbs

    putStrLn "Joint probabilities of chars"
    sortAndShow id $ jointProbs text

    putStrLn "Conditional probabilities of chars"
    sortAndShow id $ condProbs text

    --Entropy
    putStrLn $ "Entropy = " ++ (show . entropy . elems) charProbs

    --Conditional Entropy
    putStrLn $ "Conditional Entropy = " ++ (show $ fullCondEntropy text)

showColumn :: Show a => [a] -> [IO ()]
showColumn = map (\x -> putStrLn (show x))

elemFreqs :: Ord a => [a] -> Map a Double
elemFreqs = foldl' elemToFreqs (Map.empty) where
    elemToFreqs m chr = Map.alter add chr m
    add Nothing  = Just   1.0
    add (Just x) = Just $ 1.0 + x

chrFreqs = elemFreqs
