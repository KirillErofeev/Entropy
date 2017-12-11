module Stat where

import Data.List (foldl')
import qualified Data.ByteString.Lazy as BS (pack, writeFile) 
import Data.Word8
import Data.Bits (shift, (.|.))

bitSymbolRat text code = fromIntegral lengthCode / fromIntegral len where
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

