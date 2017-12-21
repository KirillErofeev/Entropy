module ShannonFano where

import Data.Tree

import Data.Word8
import qualified Data.Map as Map (fromList, toList, empty)
import Data.Map (lookupLT, Map(..), (!), unionWith, alter, foldMapWithKey)
import Data.List
import Data.Maybe

import Stat (bitSymbolRat)
import Huffman 
import Entropy (toPairs, elemProbs)
import Types
import Stat hiding (charProbs)
import qualified Data.ByteString.Lazy as BS (pack, writeFile) 

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

runSF' text = do
    --let code = sfCode text
    let pm = elemProbs text
    print pm
    let sm = splitMapOnNearProb ' ' pm
    print sm
    putStrLn "\nHuffman: "
    return ()

runSF text = do
    let code = sfCode text
    putStrLn "\nShannonFano: "
    let bsr = bitSymbolfRat text code
    BS.writeFile "data/sf" $ (BS.pack . squezze . take (round $ fromIntegral (length text) * bsr))code 
    putStrLn $ "Bits/Symbol " ++ (show bsr)  
    let pt = toPairs text
    let code = (sfCode' . toPairs) text
    putStrLn "\nShannon pair: "
    let bsr = bitSymbolCondRat'' text code
    BS.writeFile "data/condSf" $ (BS.pack . squezze . take (round $ fromIntegral (length text) * bsr))code 
    putStrLn $ "Bits/Symbol " ++ (show bsr)
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
