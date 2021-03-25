{-# OPTIONS_GHC -fwarn-tabs #-}

module IntSet (
    IntSet(..),

    empty,
    member,
    insert,
    merge,
    delete,

    fromList,
    toList
  ) where

import Data.List (foldl', nub, sort)

newtype IntSet = IntSet [(Int, Int)]
  deriving (Eq, Ord, Show)

-- | Create an empty set
empty :: IntSet
empty = IntSet []

-- | Check if an integer is in the set
member :: Int -> IntSet -> Bool
member a0 (IntSet x0) = mem a0 x0
  where
  mem :: Ord a => a -> [(a, a)] -> Bool
  mem _ [] = False 
  mem a ((x1,x2):xs) | a < x1 = False
                     | (a <= x2) && (a >= x1) = True
                     | otherwise = mem a xs

-- | Insert an element into the set
insert :: Int -> IntSet -> IntSet
insert a (IntSet []) = IntSet [(a,a)]
insert a x = merge (IntSet [(a,a)]) x

-- | Union two sets
merge :: IntSet -> IntSet -> IntSet
merge (IntSet x) (IntSet y) = IntSet (mer (sort (nub (concat [x,y]))))
  where 
  mer :: (Ord a, Num a) => [(a,a)] -> [(a,a)]
  mer xs = reverse (foldl merHelper [] xs)

merHelper :: (Ord a, Num a) => [(a,a)] -> (a,a) -> [(a,a)]
merHelper [] y = [y]
merHelper ((x0,x1):xs) (y0,y1) | (x1 + 1) == y0 = (x0,y1) : xs 
                               | x1 < y0   = (y0,y1) : (x0,x1) : xs 
                               | x1 <= y1  = (x0,y1) : xs 
                               | otherwise = (x0,x1) : xs

-- | Delete an element from the set
delete :: Int -> IntSet -> IntSet
delete a0 (IntSet x0) = IntSet (del a0 x0)
  where
  ---del :: Ord a => a -> [(a,a)] -> [(a,a)]
  del :: Int -> [(Int,Int)] -> [(Int,Int)]
  del _ [] = []
  del a ((x1,x2):xs) | a < x1 = (x1,x2):xs
                     | (x1 == x2) && (x1 == a) = xs
                     | x1 == a = ((x1 + 1) ,x2) : xs
                     | x2 == a = (x1 ,(x2-1)) : xs
                     | (a < x2) && (a > x1) = (x1,(a-1)) : ((a+1),x2) : xs
                     | otherwise = (x1,x2) : (del a xs)

-- | Convert a list of @Int@s to an @IntSet@
fromList :: [Int] -> IntSet
fromList is = foldl' (flip insert) empty is

-- | Convert an @IntSet@ to a list of @Int@s
toList :: IntSet -> [Int]
toList (IntSet xs) = nub $ sort $ concat [[lo..hi] | (lo,hi) <- xs]
