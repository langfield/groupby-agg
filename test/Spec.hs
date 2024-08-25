import Test.Hspec
import Test.Hspec.LeanCheck as LC

import Data.List (intercalate)

import Data.Word (Word32)

import System.Process
import System.IO.Unsafe (unsafePerformIO)

import Lib (run, agg, parse, Report (..))

import Debug.Trace (trace)

traceF :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceF fname f x =
  trace
    (unwords [fname, show x, "=", show (f x)])
    (f x)

-- Types for enumerating property-based testing data.
newtype Volume = Volume Word32 deriving (Eq, Ord)
data Symbol = S1 | S2 | S3 | S4 | S5 deriving (Eq, Ord, Show, Read)
data Side = Bid | Ask deriving (Eq, Ord, Show)
data Row = Row Symbol Int Volume deriving (Eq, Ord, Show)

reportSymbol :: Report -> Symbol
reportSymbol (Report s _ _) = read s

-- | We represent positive volumes as unsigned integers.
--
-- We use this slight hack to map the representations into the positive
-- integers, since we will always serialize CSV rows before passing them
-- to the functions under test.
instance Show Volume where
  show (Volume v) = show (v + 1)

-- LeanCheck instances

instance Listable Volume where
  tiers = cons1 Volume

instance Listable Symbol where
  tiers =  cons0 S1
        \/ cons0 S2
        \/ cons0 S3
        \/ cons0 S4
        \/ cons0 S5

instance Listable Row where
  tiers = cons3 Row

-- Helper functions

csvify :: Row -> String
csvify (Row s p v) = intercalate "," ["A", "A", show s, "Bid", show p, show v]

serialize :: [Row] -> String
serialize = unlines . map csvify

python :: String -> String
python s = filter (`notElem` "\n ")
         $ unsafePerformIO
         $ readProcess "python3" ["a.py"]
         $ unlines
         [ s ]

main :: IO ()
main = hspec $ do
  describe "run" $ do
    it "handles empty input" $ do
      run "" `shouldBe` "{}"

    it "handles trivial input" $ do
      run ",,A,,0,0" `shouldBe` "{\"A\":{\"vwap\":0.0,\"volume\":0}}"

    it "handles empty symbol names" $ do
      run ",,,,0,0" `shouldBe` "{\"\":{\"vwap\":0.0,\"volume\":0}}"

    it "handles zero volume correctly" $ do
      run "A,A,A,Bid,42,0" `shouldBe` "{\"A\":{\"vwap\":42.0,\"volume\":0}}"

    it "handles a single row from the sample input" $ do
      run "Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10" `shouldBe` "{\"BUSU1\":{\"vwap\":42.0,\"volume\":10}}"

    it "handles entire sample input" $ do
      run "Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10\nCHOAM Arakis Z23,OPEC 897,BUIZ1,Ask,-2,14\nInGen Tech BCZ232,BioSynFG332,BUSM2,Bid,43250,23"
        `shouldBe` "{\"BUIZ1\":{\"vwap\":-2.0,\"volume\":14},\"BUSM2\":{\"vwap\":43250.0,\"volume\":23},\"BUSU1\":{\"vwap\":42.0,\"volume\":10}}"

    it "matches a python implementation" $
      LC.propertyFor 0 $ \rows ->
        let s = unlines (map csvify rows)
            predicted = traceF "run" run s
            actual = traceF "python" python s
         in predicted == actual

  describe "agg . parse" $ do
    it "returns exactly 1 `Report` for each symbol" $
      LC.propertyFor 10000 $ \xs z@(Row sym _ _) ->
        let hits = filter (== sym) . map reportSymbol . agg . parse . serialize $ (z:xs)
         in length hits == 1

    it "is homomorphic w.r.t. concatenation along row axis" $
      LC.propertyFor 10000 $ \xs ys z@(Row sym _ _) ->
        let filterReports = filter ((== sym) . reportSymbol) . agg . parse . serialize
         in case (filterReports (z:xs), filterReports (z:ys), filterReports (z:xs ++ z:ys)) of
              ([r2], [r1], [r]) -> r1 <> r2 == r
              (_   , _   , _  ) -> True
