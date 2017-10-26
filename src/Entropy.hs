module Entropy
       (
       Primes.primes
      ,run
      ,writePrimes
       )
       where

import qualified Data.Map as Map
import Data.Map (Map, keys, elems, mapWithKey)
import Data.List
import Data.Numbers.Primes as Primes

primeStream :: [Int]
primeStream = 2 : [x | x <- [3..],
                           all (\y -> x `mod` y /= 0)          .
                           map round                           .
                           takeWhile (<= sqrt(fromIntegral x)) .
                           map fromIntegral                    $
                           primeStream]

fileName   = "primes10e3"
numberOfPrimes = 1000

writePrimes = (writeFile fileName . init . tail . show . take numberOfPrimes) primes

--chrFreq :: String -> Map Char Double
--chrFreq = foldr chrToFreqs (Map.empty) where 
--    chrToFreqs chr m = Map.alter add chr m 
--    add Nothing = Just 1.0 
--    add (Just x) = Just $ x+1.0
chrFreqs :: String -> Map Char Double
chrFreqs = foldl' chrToFreqs (Map.empty) where 
    chrToFreqs m chr = Map.alter add chr m 
    add Nothing = Just 1.0 
    add (Just x) = Just $ x+1.0

entropy = negate . sum . map (\p -> p * logBase 2 p)

data CharCond = Char :| Char deriving (Show, Eq, Read)

instance Ord CharCond where
    compare (a :| b) (a0 :| b0) = (a, b) `compare` (a0, b0)

toCharCond []  = [] 
toCharCond [x] = [] 
toCharCond (x:x0:xs) = (x :| x0) : toCharCond (x0:xs) 

chrCondProbs :: String -> Map Char Double -> Map CharCond Double
chrCondProbs text chrFreqs = 
    mapWithKey normalize $ foldl' ccToFreqs (Map.empty) (toCharCond text) where 
    ccToFreqs m cc = Map.alter add cc m 
    
    add Nothing = Just 1.0 
    add (Just x) = Just $ x+1.0
    
    normalize (_ :| cc) frq = let Just a = Map.lookup cc chrFreqs in
                                frq / a

fullCondEntropy :: Map CharCond Double -> Map Char Double -> Double
fullCondEntropy condProbs probs = (negate . sum . elems . mapWithKey f) condProbs where
    f (c :| cc) frq = let Just condProb = Map.lookup (c :| cc) condProbs
                          Just prob     = Map.lookup cc probs in
                               prob * condProb * logBase 2 condProb 

--run = fmap (normalize . chrFreq) $ readFile fileName
run = do
    text           <- readFile fileName
    let lengthText =  fromIntegral $ length text
    let charFreqs  =  chrFreqs text
    let charProbs  =  fmap (/ genericLength text) $ chrFreqs text
    --Probabilities of chars
    (sequence . showColumn                    . 
     sortBy (\(a,b) (a0, b0) -> compare b0 b) .
     Map.toList)                      charProbs
    --Conditional Probabilities of chars
    let charCondProbs  =  chrCondProbs text charFreqs
    (sequence . showColumn                    . 
     sortBy (\(a,b) (a0, b0) -> compare b0 b) .
     Map.toList)                      charCondProbs
    --Entropy
    putStrLn $ "Entropy = " ++ (show . entropy . elems) charProbs 
    --Full Conditional Entropy
    putStrLn $ "Full Conditional Entropy = " ++ (show . fullCondEntropy charCondProbs) charProbs 


showColumn :: Show a => [a] -> [IO ()]
showColumn = map (\x -> putStrLn (show x))


