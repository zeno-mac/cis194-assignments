module Week3.Golf where

{-Exercise 1 Hopscotch
Your first task is to write a function
skips :: [a] -> [[a]]
The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list-}

skips :: [a] -> [[a]]
skips x = [everyNth x n | n<- [1..length x]]
everyNth :: [a] -> Int  -> [a]
everyNth x n = acc 1 x where
    acc _ [] = []
    acc a (x:l)
        | a == n = x :acc 1 l
        | otherwise = acc (a+1) l


skips' :: [a] -> [[a]]
skips' x = map (everyNth' x) [1..length x]
everyNth' :: [a] -> Int -> [a]
everyNth' xs n = [x | (x, i) <- zip xs [1..], i `mod` n == 0]


skips'' :: [a] -> [[a]]
skips'' xs = [ [ x | (i, x) <- zip [1..] xs, i `mod` n == 0 ] | n <- [1..length xs] ]


{-Exercise 2 Local maxima
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in
order.-}

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:l)
    | a<b && b>c = b : localMaxima (b:c:l)
    | otherwise = localMaxima (b:c:l)
localMaxima _ = []


{-Wrong version, limits can be localMaxima, read the assignment wrong. To be ignored-}

higherThanNextList :: [Integer] -> [Bool]
higherThanNextList [] = []
higherThanNextList (x:l) = acc x l : higherThanNextList l where
    acc _ [] = True
    acc a (x:_) = a > x

higherThanPrevList :: [Integer] -> [Bool]
higherThanPrevList [] = []
higherThanPrevList (x:l) = True : acc x l where
    acc _ [] = []
    acc a (x:l) = (a<x) : acc x l

localMaximaWrong :: [Integer] -> [Integer]
localMaximaWrong x = [x | ((x,b1),b2) <- zip (zip x (higherThanNextList x)) (higherThanPrevList x), b1 && b2]

{-Exercise 3 Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). -}

histogram :: [Integer] -> String
histogram x = let countList = count x in acc countList where
    acc x 
     | maxValue x <= 0 = ending
     | otherwise = partialHistogram (maxValue x) x  ++ ('\n' :acc (reduceMaxValue x))

reduceMaxValue :: [Integer] -> [Integer]
reduceMaxValue x = map (acc (maxValue x)) x where
    acc n x
     | n == x = x -1
     | otherwise =  x

partialHistogram :: Integer -> [Integer] -> String
partialHistogram x  = map (acc x)  where
    acc n x
     | n == x = '*'
     | otherwise =  ' '

maxValue :: [Integer] -> Integer
maxValue x = acc 0 x where
    acc n [] = n
    acc n(x:l) 
     | n > x = acc n l
     | otherwise = acc x l
    
count :: [Integer] -> [Integer]
count = foldr incPosition ([0 | x <- [0 .. 9]])

incPosition :: Integer -> [Integer] -> [Integer]
incPosition _ [] = []
incPosition 0 (x:l) = (x+1) : l
incPosition n (x:l) = x : incPosition (n-1) l


ending :: String
ending = "==========\n0123456789\n"

