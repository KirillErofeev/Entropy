{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib
import Entropy

--import Diagrams.Backend.Cairo 
import Diagrams.Backend.Cairo.CmdLine (B, mainWith)
import Diagrams.Prelude

import qualified Data.Vector as V
import Linear.V2 as V
import Control.Lens hiding (transform, ( # ))
import Plots.Types
import Plots

main :: IO ()
--main = (writeFile "primes10e7" . init . tail . show . take 10000000) primes
main = run
