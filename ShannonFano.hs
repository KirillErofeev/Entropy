module ShannonFano where

import Data.Tree

import qualified Data.Map as Map (fromList, toList, empty)
import Data.Map (Map(..), (!), unionWith, alter, foldMapWithKey)
import Data.List

import Stat (bitSymbolRat)
import Huffman (encode)
import Entropy (toPairs)
import Types

probabilityModelToRootTree :: Ord a => ProbabilityModelP a -> Tree (Map a Double)
probabilityModelToRootTree pm = Node pm []

splitMapOnNearProb pm = (mapBoth Map.fromList . span (\(char, _) -> char /= sc) . sortPM) pm where
    sc = splitChar pm
    splitChar = (fst . head . dropWhile (\(_, p) -> p < (sumPM pm * 0.5)) . scanl (\(_, a) (c, b) -> (c, a + b)) (' ', 0.0) . sortPM)
    sortPM = sortBy cmp . Map.toList
    cmp (_, a) (_, b) | a >  b = GT
                      | a == b = EQ
                      | True   = LT

buildCodeTree charProbs = bct $ probabilityModelToRootTree charProbs where
    bct (Node pm []) = undefined

sfCode text = undefined 

runSF text = do
    --let code = sfCode text
    putStrLn "\nHuffman: "
    --putStrLn $ "Bits/Symbol " ++ (show $ bitSymbolRat text code)
    ----putStrLn $ drawTree $
    ----               (setTreesToCodeTree         .
    ----               probabilityModelToSetTrees .
    ----               charProbs) text
    ----putStrLn $ show $ (codeTreeToCode            .
    ----               setTreesToCodeTree         .
    ----               probabilityModelToSetTrees .
    ----               charProbs) text
    --let pt = toPairs text
    --let codePair = (sfCode . toPairs) text
    --putStrLn "\nHuffman pair: "
    --putStrLn $ "Bits/Symbol " ++ (show $ bitSymbolRat pt codePair / 2)
    --putStrLn "\n\n"
    return ()
