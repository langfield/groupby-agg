{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Data.List (sortBy)
import Data.List.NonEmpty (groupBy)
import Data.Aeson (ToJSON (..), (.=), object, pairs, encode)
import Data.Function (on)
import Data.Foldable1 (foldMap1)
import Data.ByteString.Lazy.Char8 (unpack)

import qualified Data.Map as M

split :: String -> [String]
split [] = []
split xs =
  case dropWhile (==',') xs of
    [] -> []
    ys -> prefix : split suffix
      where
        (prefix, suffix) = break (==',') ys

data Match = Match { symbol :: String, price :: Int, quantity :: Int } deriving (Eq, Ord, Show)
data Report = Report String Double Int deriving (Eq, Ord, Show)

instance ToJSON Report where
  toJSON (Report _ vwap vol) = object ["vwap" .= vwap, "volume" .= vol]
  toEncoding (Report _ vwap vol) = pairs $ "vwap" .= vwap <> "volume" .= vol

-- | Merge two reports with same symbol (does weighted avg price and sums volume).
instance Semigroup Report where
  (Report s p1 n1) <> (Report _ p2 n2) = Report s ((p1*q1 + p2*q2) / (q1+q2)) (n1+n2)
    where
      q1 = fromIntegral n1
      q2 = fromIntegral n2

groupAgg :: Semigroup m => (a -> m) -> (a -> a -> Bool) -> (a -> a -> Ordering) -> [a] -> [m]
groupAgg f g h = map (foldMap1 f) . groupBy g . sortBy h

mkmatch :: [String] -> Match
mkmatch [_,_,s,_,p,q] = Match s (read p) (read q)
mkmatch row = error $ "bad row: " ++ show row

mkreport :: Match -> Report
mkreport (Match s p q) = Report s (fromIntegral p) q

pair :: Report -> (String, Report)
pair r@(Report s _ _) = (s, r)

parse :: String -> [Match]
parse = map (mkmatch . split) . lines

jsonify :: [Report] -> String
jsonify = unpack . encode . M.fromList . map pair

run :: String -> String
run = jsonify . groupAgg mkreport ((==) `on` symbol) (compare `on` symbol) . parse
