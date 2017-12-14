{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Data.Char (toLower)
import Lib
import Entropy
import ArithmeticCoding
import Huffman
import ShannonFano 

fileWords = "words.txt"

main :: IO ()
main = do 
    _text <- (fmap . fmap) toLower (readFile fileWords)
    let text = take 500001 _text
    runSF text
    runHuffman text
    runAC' text 
