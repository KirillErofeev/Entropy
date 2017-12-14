module ShannonFano where

import Data.Tree

import qualified Data.Map as Map (fromList, toList, empty)
import Data.Map (lookupLT, Map(..), (!), unionWith, alter, foldMapWithKey)
import Data.List
import Data.Maybe

import Stat (bitSymbolRat)
import Huffman 
import Entropy (toPairs, elemProbs)
import Types

probabilityModelToRootTree :: Ord a => ProbabilityModelP a -> Tree (Map a Double)
probabilityModelToRootTree pm = Node pm []

splitMapOnNearProb :: Ord a => 
    a -> ProbabilityModelP a -> (ProbabilityModelP a, ProbabilityModelP a)
splitMapOnNearProb n pm = (mapBoth Map.fromList . span (\(char, _) -> char /= sc) . sortPM) pm where
    sc = splitChar pm
    splitChar = (fst . head . 
                 dropWhile (\(_, p) -> p < (sumPM pm * 0.5)) .
                 scanl (\(_, a) (c, b) -> (c, a + b)) (n, 0.0) . 
                 sortPM)
    sortPM = sortBy cmp . Map.toList
    cmp (_, a) (_, b) | a >  b = GT
                      | a == b = EQ
                      | True   = LT

buildCodeTree charProbs = bct $ probabilityModelToRootTree charProbs where
    bct (Node pm []) = undefined

sfCode text = flip encode text           $
                   (alter a' z' . 
                   codeTreeToCode            .
                   setTreesToCodeTree         .
                   probabilityModelToSetTrees .
                   elemProbs) text
                   where
    a' (Just v) = Just $ 0 : v 
    z' = fst $ fromJust $ lookupLT 'z' (elemProbs text)

test = "aaaaaaacbb"
runSF' text = do
    --let code = sfCode text
    let pm = elemProbs text
    print pm
    let sm = splitMapOnNearProb ' ' pm
    print sm
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

runSF text = do
    let code = sfCode text
    putStrLn "\nShannonFano: "
    putStrLn $ "Bits/Symbol " ++ (show $ bitSymbolRat text code)
    let pt = toPairs text
    let codePair = (sfCode' . toPairs) text
    putStrLn "\nShannon pair: "
    putStrLn $ "Bits/Symbol " ++ (show $ bitSymbolRat pt codePair / 2)
    putStrLn "\n\n"
    return ()
    
sfCode' text = flip encode text           $
                   (alter a' z' . 
                   codeTreeToCode            .
                   setTreesToCodeTree         .
                   probabilityModelToSetTrees .
                   elemProbs) text
                   where
    a' (Just v) = Just $ 1:1:0 : 0 : v 
    z' = fst $ fromJust $ lookupLT ('r' :| 'r') (elemProbs text)
