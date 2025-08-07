module Week6.HW6 where

{-Exercise 1-}
fib1 :: Integer -> Integer
fib1 0 = 1
fib1 1 = 1
fib1 n = fib1 (n-1) + fib1 (n-2)