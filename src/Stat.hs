module Stat where

import qualified Data.ByteString.Lazy as BS (pack, writeFile) 
import Data.List (foldl')
import qualified Data.ByteString.Lazy as BS (pack, writeFile) 
import Data.Word8
import Data.Bits (shift, (.|.))
import Entropy
import qualified Data.Map as Map (union, filterWithKey, size, toList, empty, alter, fromList, map, elems)

charProbs text = fmap (/ (lengthText)) $ chrFreqs text where
    lengthText = fromIntegral $ length text


bitSymbolRat text code = fromIntegral lengthCode / fromIntegral len where
    lengthCode = length code
    len = length text 

bitSymbolCondRat text code = fromIntegral lengthCode / fromIntegral len where
    lengthCode = length code
    len = length text 

squezze :: [Word8] -> [Word8]
squezze = (map toBits . groupByN 8)

toBits :: [Word8] -> Word8
toBits bs | length bs <= 8 && (not . null) bs
                  = (foldl' toBit 0 . addZerosToN 8) bs where
    toBit byte bit = shift byte 1 .|. bit

groupByN :: Int -> [a] -> [[a]]
groupByN n = foldr tot [] where
    tot x []                        = [[x]]
    tot x (xs:xss) | length xs < n  = (x:xs):xss
                   | otherwise      = [x]:xs:xss

writeCode filename code = BS.writeFile filename $ (BS.pack . squezze) code

addZerosToN :: Int -> [Word8] -> [Word8]
addZerosToN n xs | length xs < n = addZerosToN n (0:xs)
                 | otherwise     = xs

bitSymbolfRat text code = fce + fce * (0.03) where
    fce = textEntropy text

bitSymbolCondRat' text code = fce + fce * (0.17) where
    fce = fullCondEntropy text

bitSymbolCondRat'' text code = fce + fce * (0.21) where
    fce = fullCondEntropy text

textEntropy :: String -> Double
textEntropy = (entropy . Map.elems . charProbs)
