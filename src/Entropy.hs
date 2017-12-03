{-# LANGUAGE NoMonomorphismRestriction #-}
module Entropy
--       (
--       Primes.primes
--      ,run
--      ,writePrimes
--      ,swap
--       )
       where


import qualified Data.Map as Map
import Data.Map (Map, keys, elems, mapWithKey)
import Data.List
import qualified Data.Set as Set
import Data.Tuple (swap)

import Data.Numbers.Primes as Primes

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

fileName   = "data/words.txt"
edgeOfPrimes   = 100000000

writePrimes = (writeFile fileName . init . tail . show . takeWhile (< edgeOfPrimes)) primes

writeTwinPrimes = (writeFile fileName . init . tail . show . takeWhile (< edgeOfPrimes)) twinPrimesStream

entropy = negate . sum . map (\p -> p * logBase 2 p)

data CharCond = (:|) {leftCc :: Char, rightCc :: Char} deriving (Eq, Read)

instance Show CharCond where
    show (c :| cc) = show c ++ "|" ++ show cc

instance Ord CharCond where
    compare (a :| b) (a0 :| b0) = (a, b) `compare` (a0, b0)

newtype OrdFirst = OrdFirst {get :: CharCond}  deriving (Show, Eq)

instance Ord OrdFirst where
    compare (OrdFirst (a :| b)) (OrdFirst (a0 :| b0)) = a `compare` a0


rev (l :| r) = r :| l

toCharCond []  = []
toCharCond [x] = []
toCharCond (x:x0:xs) = (x :| x0) : toCharCond (x0:xs)

normalizePrev chrFreqs (_ :| cc) frq = let
                              Just a = Map.lookup cc chrFreqs
                          in
                              frq / a

normalizeNext chrFreqs (c :| _) frq = let
                              Just a = Map.lookup c chrFreqs
                          in
                              frq / a

emptyMap allChars = Map.fromList [(cs :| csNext, 0) | cs <- allChars, csNext <- allChars]

chrCondProbs :: (CharCond -> Double -> Double) -> String -> Map CharCond Double
chrCondProbs normalize text = mapWithKey normalize $ foldl' ccToFreqs (emptyMap allChars) (toCharCond text) where
    allChars = Set.toAscList . Set.fromList $ text

    ccToFreqs m cc = Map.alter add cc m

    add (Just x) = Just $ 1.0 + x

chrCondPrev chrFreqs = chrCondProbs (normalizePrev chrFreqs)
chrCondNext chrFreqs = chrCondProbs (normalizeNext chrFreqs)


fullCondEntropy :: Map CharCond Double -> Map Char Double -> Double
fullCondEntropy condProbs probs = (negate . sum . elems . mapWithKey f) condProbs where

    f (c :| cc) frq = let
                          Just condProb = Map.lookup (c :| cc) condProbs
                          Just prob     = Map.lookup cc probs
                      in
                          if abs condProb > 1e-17
                              then prob * condProb * logBase 2 condProb
                              else 0

mapTo2D :: Map CharCond Double -> [[Double]]
mapTo2D = (map . map $ snd) . (foldr to2d []) . (Map.toList) where
    to2d elem []       = [[elem]]
    to2d elem (l@(x:xs):xss) | (leftCc . fst) elem  == (leftCc . fst) x = (elem : l) : xss
    to2d elem (l@(x:xs):xss) | otherwise                                = [elem] : l : xss

runEntropy = do
    text           <- (fmap $ filter (/= ',')) (readFile fileName)
    --text           <- readFile fileName
    let lengthText =  fromIntegral $ length text
    let charFreqs  =  chrFreqs text
    let charProbs  =  fmap (/ genericLength text) $ chrFreqs text
    let sortAndShow = (\f -> sequence_ . showColumn . fmap f . sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList)

    putStrLn "Probabilities of chars"
    sortAndShow id charProbs

    --putStrLn "Probabilities of chars on condition, that next one is known"
    --let charCondNextProbs  =  chrCondPrev charFreqs text
    --sortAndShow id charCondNextProbs
    --putStrLn ""

    --putStrLn "Probabilities of chars on condition, that pevious one is known"
    --let charCondPrevProbs  =  chrCondNext charFreqs text
    ----sortAndShow (\(cc, d) -> (rev cc, d)) charCondPrevProbs
    --putStrLn ""

    ----Entropy
    --putStrLn $ "Entropy = " ++ (show . entropy . elems) charProbs
    ----Full Conditional Entropy
    --putStrLn $ "Full Conditional on previous Entropy = " ++ (show . fullCondEntropy charCondPrevProbs) charProbs
    ----Full Conditional Entropy
    --putStrLn $ "Full Conditional on next Entropy = " ++ (show . fullCondEntropy charCondNextProbs) charProbs

showColumn :: Show a => [a] -> [IO ()]
showColumn = map (\x -> putStrLn (show x))

chrFreqs :: String -> Map Char Double
chrFreqs = foldl' chrToFreqs (Map.empty) where

    chrToFreqs m chr = Map.alter add chr m

    add Nothing  = Just   1.0
    add (Just x) = Just $ 1.0 + x
