{-# LANGUAGE FlexibleInstances #-}
module Types where

import Data.Tree
import Data.Set (Set(..))
import Data.Map (Map(..))
import qualified Data.Map as Map (fromList, map, alter)
import Data.List 
import Data.Word8

type ProbabilityModel     = Map Char Double
type CondProbabilityModel = Map CharCond Double
type Intervals            = Map Char Interval
type CondIntervals        = Map CharCond Interval
type Interval             = (Double, Double)
type Code                 = Map Char [Word8]
type CodeTree             = Tree [Char]
type SetProbabilityTrees  = Set (Tree ([Char], Double))

instance Ord a => Ord (Tree a) where
    compare (Node a as) (Node b bs) = compare (a,as) (b,bs) 
    --compare (Node a []) (Node b []) = compare a b 
    --compare (Node a as) (Node b []) | compare a b == EQ = GT
    --                                | otherwise         = LT       
    --compare (Node a []) (Node b bs) | compare a b == EQ = LT 
    --                                | otherwise         = GT
    --compare (Node a as) (Node b bs) | compare a b == EQ = compare as bs
    --                                | otherwise         = compare a b

newtype ProbOrderedTree = POT {getTree :: (Tree ([Char], Double))}

instance Eq ProbOrderedTree where
    (==) (POT (Node (_, a) _ )) (POT ((Node (_, b) _) )) = a == b

instance Ord ProbOrderedTree where
    compare (POT (Node (csl, l) _ )) (POT ((Node (csr, r) _) )) = compare (l, csl) (r, csr)

data AdaptMap k v = AdaptMap {getMap :: (Map k v), getSumWeight :: Double}

initAdaptMap :: String -> AdaptMap Char Double
initAdaptMap text = AdaptMap
            (Map.fromList $ zip (nubText) (repeat 1.0)) (fromIntegral $ length nubText) where
    nubText = nub text

updateAdaptMap (AdaptMap m w) char = AdaptMap (Map.alter up char m) (w+1.0) where
    up (Just frq) = Just $ frq + 1.0


normAdaptMap (AdaptMap m w) = Map.map (/w) m

belong :: Double -> Interval -> Bool
belong v (l,r) = v > l && v <= r

isSup v (_,r) = v > r
isInf v (l,_) = v <= l

dif (l, r) = r - l

mapInvl (wl, wr) (chl, chr) = ((wr - wl) * chl + wl, (wr - wl) * chr + wl)

binCenter (l, r) = (0.5 - l) / (r - l)

mapBoth fun (f,s) = (fun f, fun s)

norm p = mapBoth (/ dif p) p

updateWorkInvl1 = mapBoth (\x -> (x - 0.5)*2.0)
updateWorkInvl0 = mapBoth (*2.0)

data CharCond = (:|) {leftCc :: Char, rightCc :: Char} deriving (Eq, Read)

instance Show CharCond where
    show (c :| cc) = show c ++ "|" ++ show cc

instance Ord CharCond where
    compare (a :| b) (a0 :| b0) = (a, b) `compare` (a0, b0)

newtype OrdFirst = OrdFirst {get :: CharCond}  deriving (Show, Eq)

instance Ord OrdFirst where
    compare (OrdFirst (a :| b)) (OrdFirst (a0 :| b0)) = a `compare` a0
