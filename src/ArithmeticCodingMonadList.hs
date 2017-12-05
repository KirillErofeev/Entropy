{-# LANGUAGE NoMonomorphismRestriction #-}
module ArithmeticCoding where

import Data.List (sortBy, foldl', (\\))
import Data.Char (toLower)
import Data.Map (Map(..), (!))
import qualified Data.Map as Map (toList, empty, alter, fromList)
import Data.Ratio (numerator, denominator, (%))
import Data.Maybe (fromJust)
import Data.Word8 (Word8)

import Data.Numbers.Primes as Primes

import Entropy (chrFreqs)

fileWords = "data/words.txt"

type ProbabilityModel = Map Char Double
type Intervals        = Map Char (Double, Double)

type Interval         = (Double, Double)

belong :: Double -> Interval -> Bool
belong v (l,r) = v > l && v <= r

isSup v (_,r) = v > r
isIff v (l,_) = v <= l

data EvalState = EvalState { code :: [Word8]
                           , interval :: Interval
                           , conds :: [Interval -> Bool]}

instance Show EvalState where
    show (EvalState code inl _) = "{" ++ show code ++ ", " ++ show inl ++ "}"

charProbs :: String -> ProbabilityModel 
charProbs text = fmap (/ (fromIntegral $ length text)) $ chrFreqs text

charProbsToProbModel :: ProbabilityModel -> Intervals
charProbsToProbModel cp = Map.fromList $ foldr toInvls [] (Map.toList cp) where
    toInvls (key, pr) []                  = [(key, (0, pr))]
    toInvls (key, pr) invls@((_,(_,r)):_) = (key, (r, r+pr)) : invls

testString = "Sai"

arithmeticCoding :: Intervals -> String -> [Word8]
arithmeticCoding invls text = code . head $ ac invls text $ EvalState [] (0,1) []

ac :: Intervals -> String -> EvalState -> [EvalState]
ac invls [] es = return $ es {code = 1 : code es}
ac invls (sym:text) (EvalState bytes (wl, wr) []) = do
    let (cl, cr) = invls ! sym
    let dif = wr - wl
    let nwInl@(nwl, nwr) = (wl + dif * cl, wl + dif * cr)
    case (isInf, isSup) of
        (True, False) -> return $  


--codeToBytes :: Interval -> ByteString
--codeToBytes 

{-- View --}
mapToSortBySndList :: ProbabilityModel -> [(Char, Double)]
mapToSortBySndList = sortBy (\(a,b) (a0, b0) -> compare b0 b) . Map.toList

showColumnList :: Show a => [a] -> IO ()
showColumnList = sequence_ . map (\x -> putStrLn (show x))
{-- View (end) --}

runAC = do
    text <- (readFile fileWords)
    print text

runAC' = do
    text <- (fmap . fmap) toLower (readFile fileWords)
    let text' = take 17 text
    let probs = charProbs text'
    let intervals  = charProbsToProbModel probs
    --let inl        = arithmeticCoding intervals text'
    print text'
    showColumnList $ mapToSortBySndList probs
    showColumnList $ Map.toList intervals
    --print $ arithmeticCoding intervals text'
    return () 
