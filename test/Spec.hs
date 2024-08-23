import Test.Hspec

import Lib (run)

main :: IO ()
main = hspec $
  describe "run" $ do
    it "handles empty input" $ do
      run "" `shouldBe` "{}"
    it "handles a single row from the sample input" $ do
      run "Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10" `shouldBe` "{\"BUSU1\":{\"vwap\":42.0,\"volume\":10}}"
    it "handles entire sample input" $ do
      run "Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10\nCHOAM Arakis Z23,OPEC 897,BUIZ1,Ask,-2,14\nInGen Tech BCZ232,BioSynFG332,BUSM2,Bid,43250,23"
        `shouldBe` "{\"BUIZ1\":{\"vwap\":-2.0,\"volume\":14},\"BUSM2\":{\"vwap\":43250.0,\"volume\":23},\"BUSU1\":{\"vwap\":42.0,\"volume\":10}}"
