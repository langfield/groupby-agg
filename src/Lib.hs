{-# LANGUAGE OverloadedStrings #-}
module Lib (run, agg, parse, Report(..)) where

import GHC.Data.List.SetOps (equivClasses)
import Data.Aeson (ToJSON (..), (.=), object, pairs, encode)
import Data.Function (on)
import Data.Foldable1 (foldMap1)
import Data.ByteString.Lazy.Char8 (unpack)

import qualified Data.Map as M

-- | Comma-split a string.
split :: String -> [String]
split [] = []
split xs = prefix : split (drop 1 suffix)
  where
    (prefix, suffix) = break (==',') xs

data Match = Match { symbol :: String, price :: Int, quantity :: Int } deriving (Eq, Ord, Show)
data Report = Report String Double Int deriving (Eq, Ord, Show)

instance ToJSON Report where
  toJSON (Report _ vwap vol) = object ["vwap" .= vwap, "volume" .= vol]
  toEncoding (Report _ vwap vol) = pairs $ "vwap" .= vwap <> "volume" .= vol

-- | Merge two reports with same symbol (does weighted avg price and sums volume).
instance Semigroup Report where
  r1@(Report s1 p1 n1) <> r2@(Report s2 p2 n2) =
    if s1 == s2
       then Report s1 ((p1*v1 + p2*v2) / (v1+v2)) (n1+n2)
       else error $ "Can't (<>) reports with distinct symbols: " ++ show r1 ++ ", " ++ show r2
    where
      v1 = fromIntegral n1
      v2 = fromIntegral n2

-- | Generic groupby-aggregation on lists of semigroup elements.
groupAgg :: Semigroup m => (a -> m) -> (a -> a -> Ordering) -> [a] -> [m]
groupAgg f g = map (foldMap1 f) . equivClasses g

mkmatch :: [String] -> Match
mkmatch [_,_,s,_,p,v] = Match s (read p) (read v)
mkmatch row = error $ "bad row: " ++ show row

mkreport :: Match -> Report
mkreport (Match s p v) = Report s (fromIntegral p) v

pair :: Report -> (String, Report)
pair r@(Report s _ _) = (s, r)

parse :: String -> [Match]
parse = map (mkmatch . split) . lines

-- | Serialize a list of reports as a JSON object.
jsonify :: [Report] -> String
jsonify = unpack . encode . M.fromList . map pair

-- | Aggregate a table of matches into a list of reports, one for each symbol.
agg :: [Match] -> [Report]
agg = groupAgg mkreport (compare `on` symbol)

-- | Parse a CSV of trade matches and return a JSON string of per-symbol reports.
run :: String -> String
run = jsonify . agg . parse
