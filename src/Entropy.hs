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
import Data.Tuple (swap)

import Data.Numbers.Primes as Primes

import Diagrams.Backend.Cairo.CmdLine (B, mainWith)
import Diagrams.Prelude

import qualified Data.Vector as V
import Linear.V2 as V
import Control.Lens hiding (transform, ( # ))
import Plots.Types
import Plots


primeStream :: [Int]
primeStream = 2 : [x | x <- [3..],
                           all (\y -> x `mod` y /= 0)          .
                           map round                           .
                           takeWhile (<= sqrt(fromIntegral x)) .
                           map fromIntegral                    $
                           primeStream]

fileName   = "primes less than 10e5"
edgeOfPrimes   = 100000

writePrimes = (writeFile fileName . init . tail . show . takeWhile (< edgeOfPrimes)) primes

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

emptyMap = Map.fromList [(cs :| csNext, 0) | cs <- ',' : ['0'..'9'], csNext <- ',' : ['0'..'9']]

chrCondProbs :: (CharCond -> Double -> Double) -> String -> Map CharCond Double
chrCondProbs normalize text = mapWithKey normalize $ foldl' ccToFreqs (emptyMap) (toCharCond text) where 

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
                          prob * condProb * logBase 2 condProb 

mapTo2D :: Map CharCond Double -> [[Double]]
mapTo2D = (map . map $ snd) . (foldr to2d []) . (Map.toList) where
    to2d elem []       = [[elem]]
    to2d elem (l@(x:xs):xss) | (leftCc . fst) elem  == (leftCc . fst) x = (elem : l) : xss
    to2d elem (l@(x:xs):xss) | otherwise                                = [elem] : l : xss

run = do
    text           <- readFile fileName
    let lengthText =  fromIntegral $ length text
    let charFreqs  =  chrFreqs text
    let charProbs  =  fmap (/ genericLength text) $ chrFreqs text
    let sortAndShow = (\f -> sequence . showColumn . fmap f . sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList) 

    putStrLn "Probabilities of chars"
    --sortAndShow id charProbs

    putStrLn "Probabilities of chars on condition, that next one is known"
    let charCondNextProbs  =  chrCondPrev charFreqs text
    sortAndShow id charCondNextProbs
    putStrLn ""

    putStrLn "Probabilities of chars on condition, that pevious one is known"
    let charCondPrevProbs  =  chrCondNext charFreqs text
    --sortAndShow (\(cc, d) -> (rev cc, d)) charCondPrevProbs
    putStrLn ""

    --Entropy
    putStrLn $ "Entropy = " ++ (show . entropy . elems) charProbs 
    --Full Conditional Entropy
    putStrLn $ "Full Conditional on previous Entropy = " ++ (show . fullCondEntropy charCondPrevProbs) charProbs
    --Full Conditional Entropy
    putStrLn $ "Full Conditional on next Entropy = " ++ (show . fullCondEntropy charCondNextProbs) charProbs

    sequence . showColumn $ mapTo2D charCondNextProbs
    plot $ mapTo2D charCondNextProbs

plot xs = r2AxisMain $ heatMapAxis xs

heatMapAxis :: [[Double]] -> Axis B V.V2 Double
heatMapAxis xs = r2Axis &~ do
  display colourBar
  axisExtend .= noExtend
  heatMap xs $ heatMapSize .= V.V2 10 10

showColumn :: Show a => [a] -> [IO ()]
showColumn = map (\x -> putStrLn (show x))

chrFreqs :: String -> Map Char Double
chrFreqs = foldl' chrToFreqs (Map.empty) where 

    chrToFreqs m chr = Map.alter add chr m 

    add Nothing  = Just   1.0 
    add (Just x) = Just $ 1.0 + x
