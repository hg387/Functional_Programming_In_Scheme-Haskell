module IntSetUnitSpec (
    spec
  ) where

import Test.Hspec

import IntSet

spec :: Spec
spec = do

  describe "empty" $ do
    it "empty IntSet" $
      empty `shouldBe` IntSet []

  describe "member" $ do
    it "member test for empty IntSet" $
      member 9 (IntSet []) `shouldBe` False
    it "member test for 1st interval" $
      member 4 (IntSet [(1,4),(5,10),(11,14)]) `shouldBe` True
    it "member test for last interval" $
      member 12 (IntSet [(1,4),(5,10),(11,14)]) `shouldBe` True
    it "member test for mid interval" $
      member 8 (IntSet [(1,4),(5,10),(11,14)]) `shouldBe` True
    it "member test for negative interval" $
      member (-2) (IntSet [((-3),(-1)),(1,4),(5,10),(11,14)]) `shouldBe` True
    it "member test for In-between False" $
      member 5 (IntSet [(1,4),(6,10),(11,14)]) `shouldBe` False

  describe "delete" $ do
    it "delete test for empty IntSet" $
      delete 9 (IntSet []) `shouldBe` IntSet []
    it "delete test for 1st interval" $
      delete 4 (IntSet [(1,4),(5,10),(11,14)]) `shouldBe` IntSet [(1,3),(5,10),(11,14)]
    it "delete test for mid with single element interval" $
      delete 5 (IntSet [(1,4),(5,5),(11,14)]) `shouldBe` IntSet [(1,4),(11,14)]
    it "delete test for last interval" $
      delete 13 (IntSet [(1,4),(5,5),(11,14)]) `shouldBe` IntSet [(1,4),(5,5),(11,12),(14,14)]
    it "delete test for In-between False" $
      delete 5 (IntSet [(1,2),(3,4),(6,10),(11,14)]) `shouldBe` IntSet [(1,2),(3,4),(6,10),(11,14)]
    it "delete test for negative interval" $
      delete (-1) (IntSet [(-3,-2),(1,2),(3,4),(6,10),(11,14)]) `shouldBe` IntSet [(-3,-2),(1,2),(3,4),(6,10),(11,14)]

  describe "merge" $ do
    it "merge test for first empty IntSet" $
        merge (IntSet []) (IntSet [(1,5),(8,10),(11,15)]) `shouldBe` IntSet [(1,5),(8,15)]
    it "merge test for second empty IntSet" $
        merge (IntSet [(1,5),(8,10),(11,15)]) (IntSet []) `shouldBe` IntSet [(1,5),(8,15)]
    it "merge test for duplicate IntSet pairs" $
        merge (IntSet [(1,5),(6,6),(8,10)]) (IntSet [(1,5),(6,6),(8,10)]) `shouldBe` IntSet [(1,6),(8,10)]
    it "merge test for small IntSet pairs" $
        merge (IntSet [(1,5),(8,10)]) (IntSet [(6,7),(9,10)]) `shouldBe` IntSet [(1,10)]
    it "merge test for IntSet pairs contaning single element intervals" $
        merge (IntSet [(1,5),(6,6),(8,10)]) (IntSet [(6,7),(8,8),(9,10),(11,15)]) `shouldBe` IntSet [(1,15)]
    it "merge test for IntSet pairs contaning more than five intervals each with mix intervals" $
        merge (IntSet [(1,5),(6,6),(8,10),(11,12),(20,25),(26,26),(30,37)]) (IntSet [(6,7),(8,8),(9,10),(11,15),(16,20),(21,25),(28,28)]) `shouldBe` IntSet [(1,26),(28,28),(30,37)]

  describe "insert" $ do
    it "inserting into empty set produces singleton set" $
      insert 1 empty `shouldBe` fromList [1]
    it "inserting into set at first location" $
      insert (-1) (IntSet [(0,1),(5,7)]) `shouldBe` IntSet [(-1,1),(5,7)]
    it "inserting into set at last location" $
      insert 14 (IntSet [(0,1),(3,5),(6,6),(7,9),(10,13),(14,16)]) `shouldBe` IntSet [(0,1),(3,16)]
    it "inserting into set at mid location" $
      insert 7 (IntSet [(0,1),(3,5),(6,6),(8,9),(10,13),(14,16)]) `shouldBe` IntSet [(0,1),(3,16)]  

  
    


