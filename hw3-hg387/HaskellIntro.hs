{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit x = rem x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = quot x 10 

toDigits :: Integer -> [Integer] 
toDigits x  =  if x <= 0 
	       then []
	       else toDigits (dropLastDigit (x)) ++ [lastDigit (x)]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherHelper (reverse x) 0)

doubleEveryOtherHelper :: [Integer] -> Integer -> [Integer] 
doubleEveryOtherHelper [] i     = []
doubleEveryOtherHelper (x:xs) i = if (rem i 2) == 1
				  then (2 * x) : doubleEveryOtherHelper xs (i+1)
				  else x : doubleEveryOtherHelper xs (i+1)

sumDigits :: [Integer] -> Integer
sumDigits x = sumDigitsHelper (reverse x) 0 0

sumDigitsHelper :: [Integer] -> Integer -> Integer -> Integer
sumDigitsHelper [] i acc = acc
sumDigitsHelper (x:xs) i acc = 	if (rem i 2) == 1
				then sumDigitsHelper xs (i+1) ((quot x 10) + (rem x 10) + acc)
				else sumDigitsHelper xs (i+1) (x + acc)

validate :: Integer -> Bool
validate x = (rem (sumDigits (doubleEveryOther (toDigits x))) 10) == 0 

--
-- Problem 2
--

pow :: (a->a) -> Int -> (a->a)
pow f 0 = \x -> x
---pow f x = pow f (x - 1)
pow f x = f . pow f (x - 1)
---pow f 0 a = a
---pow f x y = pow f (x - 1) (f y)

g :: Integer -> Integer

g 0 = 0
g n = n - pow g 2 (n - 1)

h :: Integer -> Integer

h 0 = 0
h n = n - pow h 3 (n - 1)

d :: Int -> Integer -> Integer
d _ 0 = 0
d i n = n - pow (d i) i (n-1)

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
