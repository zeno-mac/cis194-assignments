
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
module Week1.CreditCard where
--import Distribution.Simple.Utils (xargs)

{- Exercise 1 We need to first find the digits of a number. Define the
functions
toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits should convert positive Integers to a list of digits. (For 0 or
negative inputs, toDigits should return the empty list.) toDigitsRev
should do the same, but with the digits reversed -}



toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)
{-
O(n^2) Time complexity because of '++' operator
toDigitsRev 0 = []
toDigitsRev x
    | x<= 0 = []
    | otherwise = x `mod` 10 : toDigitsRev (x `div` 10)
-}

--Accumulator version and no auxiliary function
toDigitsRev' :: Integer -> [Integer]
toDigitsRev' n
    | n <= 0    = []
    | otherwise = reverse (go n [])
  where
    go 0 acc = acc
    go x acc = go (x `div` 10) ((x `mod` 10) : acc)


toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits x
    | x<= 0 = []
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]



{-Exercise 2 Once we have the digits in the proper order, we need to
double every other one. Define a function
doubleEveryOther :: [Integer] -> [Integer]-}

--Takes in input the reversed list and return the reversed list
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [a] = [a]
doubleEveryOtherRev (a:b:c) = a : 2*b : doubleEveryOtherRev c

--Uses the auxiliary function to reverse locally the function
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleAux (reverse x))
    where
        doubleAux [] = []
        doubleAux [x] = [x]
        doubleAux (a:b:c) = a : 2*b : doubleAux c


{-Exercise 3 The output of doubleEveryOther has a mix of one-digit
and two-digit numbers. Define the function
sumDigits :: [Integer] -> Integer-}

sumDigitsNumber :: Integer -> Integer
sumDigitsNumber x
    | x<10 = x
    | otherwise =  x `mod` 10 + sumDigitsNumber (x `div` 10)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigitsNumber x + sumDigits xs

{-Exercise 4 Define the function
validate :: Integer -> Bool-}

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigits x))`mod` 10 == 0


{-Exercise 5 - Tower of Hanoi-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi x a b c = hanoi (x-1) a c b ++ [(a,b)] ++ hanoi (x-1) c b a

