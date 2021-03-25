module IntSetPropertySpec (
    spec
  ) where

import Data.List
import Test.Hspec
import Test.QuickCheck

import IntSet (IntSet(..))
import qualified IntSet as IS

instance Arbitrary IntSet where
  arbitrary = sized (\s -> arbIntSet s 0)
    where
      arbIntSet :: Int -- ^ Size
                -> Int -- ^ Lower bound
                -> Gen IntSet
      arbIntSet 0    _  = return $ IntSet []
      arbIntSet size lb = frequency [(1,    return $ IntSet []),
                                     (size, arbIntSetRec)]
        where
          arbIntSetRec :: Gen IntSet
          arbIntSetRec = do
              NonNegative lo    <- fmap (`mod` 100) <$> arbitrary
              NonNegative delta <- fmap (`mod` 100) <$> arbitrary
              IntSet xs <- arbIntSet (size-1) (lo+delta+lb+2)
              return $ IntSet ((lo+lb,lo+delta+lb):xs)

  shrink (IntSet xs) = map IntSet $ removeOne xs ++ shrinkOne xs
    where
      removeOne :: [a] -> [[a]]
      removeOne []     = []
      removeOne [x]    = [[]]
      removeOne (x:xs) = xs : map (x:) (removeOne xs)

      shrinkOne :: [(Int,Int)] -> [[(Int,Int)]]
      shrinkOne []                       = []
      shrinkOne ((lo,hi):xs) | hi == lo  = shrinkOne xs
                             | otherwise = ((lo+1,hi):xs) :
                                           ((lo,hi-1):xs) :
                                           map ((lo,hi):) (shrinkOne xs)

spec :: Spec

spec = do
  describe "IntSet Property Tests" $ do
    it "valid: Arbitrary IntSet is valid" $
      property prop_valid
    it "empty: model of empty set is empty" $
      property prop_empty
    it "member: x is a member of set iff it is a member of the model" $
      property prop_member
    it "merge: merge of two sets equal to merge of model" $
      property prop_merge
    it "insert: inserting into set equivalent to inserting into model" $
      property prop_insert
    it "delete: deleting from set equivalent to deleting from model" $
      property prop_delete

-- | Model an IntSet by a list of Ints in the set
model :: IntSet -> [Int]
model (IntSet xs) = normalize $ concat [[lo..hi] | (lo,hi) <- xs]

-- | Normalize a list of integers so that it is sorted without duplicates
normalize :: [Int] -> [Int]
normalize = nub . sort

-- | The IntSet invariant from the problem description
invariant :: IntSet -> Bool
invariant (IntSet xs) =
    all (\(lo,hi) -> lo <= hi) xs &&
    all (\((_,hi), (lo,_)) -> lo > hi + 1) (xs `zip` drop 1 xs)

-- | Find element in the list and return boolean
find2 :: Int -> [Int] -> Bool
find2 _ [] = False
find2 a (x:xs) | x == a = True
               | otherwise = find2 a xs

prop_valid :: IntSet -> Bool
prop_valid xs = invariant xs

prop_empty :: Bool
prop_empty = (model IS.empty) == []

prop_member :: Int -> IntSet -> Bool
prop_member x xs = (IS.member x xs) == find2 x (model xs)

prop_merge :: IntSet -> IntSet -> Bool
prop_merge xs ys = ((model (IS.merge xs ys)) ==  normalize (concat [(model xs),(model ys)])) && (invariant (IS.merge xs ys))

prop_insert :: Int -> IntSet -> Bool
prop_insert x xs = ((model (IS.insert x xs)) == normalize (x : (model xs)))  && (invariant (IS.insert x xs))

prop_delete :: Int -> IntSet -> Bool
prop_delete x xs = ((model (IS.delete x xs)) ==  delete x (model xs))  && (invariant (IS.delete x xs))
