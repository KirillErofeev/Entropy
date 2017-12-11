module Huffman where

import qualified Data.Set as Set (insert, toList, map, size, foldr, union, empty, singleton)
import Data.Set (Set(..),findMin, deleteMin, deleteFindMin)
import qualified Data.Map as Map (empty)
import Data.Map (Map(..), (!), unionWith, alter, foldMapWithKey)
import Types (ProbOrderedTree(..), Code, ProbabilityModel, CodeTree, SetProbabilityTrees)
import Data.Tree
import ArithmeticCoding (charProbs)
import Data.Word8
import Stat (bitSymbolRat)

encode :: Code -> String -> [Word8]
encode code = concatMap (code !)

probabilityModelToSetTrees :: ProbabilityModel -> Set (Tree ([Char], Double))
probabilityModelToSetTrees = foldMapWithKey toSet where
    toSet char pr = Set.singleton $ Node (return char, pr) []

setTreesToCodeTree :: SetProbabilityTrees -> CodeTree
setTreesToCodeTree trees 
    | Set.size trees == 1 = (head . Set.toList) $ (Set.map . fmap) fst trees
    | Set.size trees > 1  =
        let
            (treesWoutMins, (lt@(Node l _), rt@(Node r _))) = extract2Min trees 
            newTrees = Set.insert (Node (fst l ++ fst r, snd l + snd r) [lt, rt]) treesWoutMins 
        in
            setTreesToCodeTree newTrees
    | otherwise       = error "ERROR!!! Empty Set in setTreesToCodeTree!!"
        where
    extract2Min trees = let 
                            min0   = findMin   $ Set.map POT trees
                            trees0 = deleteMin $ Set.map POT trees
                            min1   = findMin                 trees0
                            trees1 = deleteMin               trees0
                        in 
                            (Set.map getTree trees1, (getTree min0, getTree min1))

codeTreeToCode :: CodeTree -> Code
codeTreeToCode tree = h (-1) tree where
    h b (Node cs ts) = foldr mapAdd (ini cs b) (map (uncurry h) $ zip [0..] ts) where
    mapAdd = unionWith (++)
    ini cs b = foldr (add b) Map.empty cs
    add b char = alter (add' b) char
    add' (-1)  v     = v
    add' b Nothing   = Just $ [b]
    add' b (Just bs) = Just $ b:bs

huffmanCode :: String -> [Word8]
huffmanCode text = flip encode text           $
                   (codeTreeToCode            .
                   setTreesToCodeTree         .
                   probabilityModelToSetTrees .
                   charProbs) text

test = "aaaaaaccbb"
runHuffman text = do
    let code = huffmanCode text
    putStrLn "Huffman: "
    putStrLn $ "Bits/Symbol " ++ (show $ bitSymbolRat text code)
    --putStrLn $ drawTree $
    --               (setTreesToCodeTree         .
    --               probabilityModelToSetTrees .
    --               charProbs) text
    --putStrLn $ show $ (codeTreeToCode            .
    --               setTreesToCodeTree         .
    --               probabilityModelToSetTrees .
    --               charProbs) text
    return ()
