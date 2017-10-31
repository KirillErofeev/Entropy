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
import Plots.Axis
import Plots.Types
import Plots

heatMapAxis :: Axis B V.V2 Double
heatMapAxis = r2Axis &~ do
  display colourBar
  axisExtend .= noExtend

  let xs = [[1,2,3],[4,5,6]]
  heatMap xs $ heatMapSize .= V.V2 10 10--    renderSVG "1.svg" (Dims 480 480) $ showPlot p ||| strutX 0.2 ||| alignB legend

heatMapExample = renderAxis heatMapAxis

main :: IO ()
--main = (writeFile "primes10e7" . init . tail . show . take 10000000) primes
main = r2AxisMain heatMapAxis
