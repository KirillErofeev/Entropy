{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib
import Entropy
import ArithmeticCoding

main :: IO ()
main = runAC'
--main = (writeFile "primes10e7" . init . tail . show . take 10000000) primes
