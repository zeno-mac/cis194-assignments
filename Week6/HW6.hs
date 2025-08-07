{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Week6.HW6 where

{-Exercise 1
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer]
-}
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

{-Exerxise 2
Specifically, define the infinite list
fibs2 :: [Integer]
so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. 
-}
fibs2' :: Integer -> Integer -> [Integer]
fibs2' a b = a : fibs2' b (a+b)

fibs2 :: [Integer]
fibs2 = fibs2' 0 1

{-Exercise 3
Stream-}
--- Define a data type of polymorphic streams, Stream
data Stream a = C a (Stream a)

--- Write a function to convert a Stream to an infinite list
streamToList :: Stream a -> [a]
streamToList (C x xs) = x : streamToList xs

infStream' a = C a (infStream' (a+1))
infStream = infStream' 0
show' :: (Show a) => Stream a -> String
show' = showNum 20 where
    showNum 1 (C x _) = show x
    showNum n (C x xs) = show x ++ ',' : showNum (n-1) xs

instance (Show a) => Show (Stream a) where
    show :: Show a => Stream a -> String
    show =  show . take 20 . streamToList

{-Exercise 4-}

streamRepeat :: a -> Stream a
streamRepeat a = C a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (C a xs) = C (f a) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = C a (streamFromSeed f (f a))

{-Exercise 5-}

nats :: Stream Integer
nats = streamFromSeed (+1) 0

--- Technically correct but causes a Stack Overflow due to the lack of lazy evaluation
interleaveStreams' :: Stream a -> Stream a -> Stream a
interleaveStreams' (C a as) (C b bs) = C a $ C b $ interleaveStreams' as bs

--- Better version
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (C a as) b = C a (interleaveStreams b as)

ruler' :: Num t => t -> Stream t
ruler' a = interleaveStreams' (streamRepeat a) (ruler' (a+1))

ruler :: Stream Integer
ruler = ruler' 0


{-Exercise 6-}

x :: Stream Integer
x = C 0 $ C 1 $ streamRepeat 0

instance Num (Stream Integer) where
    (+) (C a as) (C b bs) = C (a+b) (as +bs)
    negate (C x xs) = C (negate x) (negate xs)
    fromInteger x = C x (streamRepeat 0)
    (*) (C a as) b'@(C b bs) = C (a*b) (streamMap (*a) bs + (as * b' ))

instance Fractional (Stream Integer) where
-- A/B = Q = (a0/b0) + x((1/b0)(A′ − QB′))
    (/) (C y ys) (C z zs) = q where
        q = C (y `div` z) (streamMap (`div` z) (ys - q * zs))

one = C 1 (streamRepeat 0)

fibs3 :: Stream Integer
fibs3 = x / (one - x - x^2)

data Matrix a = Matrix a a a a deriving (Show, Eq)

instance (Num a) => Num (Matrix a) where
    (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) = Matrix (a11*b11 + a12*b21)
                                                                   (a11*b12 + a12*b22)
                                                                   (a21*b11 + a22*b21)
                                                                   (a21*b12 + a22*b22)

fibs4 x = m12 (Matrix 1 1 1 0^x) where
    m12 (Matrix _ x _ _) = x