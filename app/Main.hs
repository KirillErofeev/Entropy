module Main where

import Lib
import Entropy

main :: IO ()
main = (writeFile "primes10e7" . init . tail . show . take 10000000) primes
