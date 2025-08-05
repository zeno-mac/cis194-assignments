{-# LANGUAGE TemplateHaskell #-}
module Week4.HW4 where
import Data.List ((\\))
{-
Exercise 1: Wholemeal programming
Reimplement each of the following functions in a more idiomatic
Haskell style. Use wholemeal programming practices, breaking each
function into a pipeline of incremental transformations to an entire
data structure. Name your functions fun1’ and fun2’ respectively.-}

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

--First possibility
fun1' :: [Integer] -> Integer
fun1'  = product . map (subtract 2) . filter even
--Second possibility
fun1'' :: [Integer] -> Integer
fun1'' xs = product [x-2 | x <- xs, even x]

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) .iterate acc where
    acc n
     | even n = div n 2
     | otherwise = 3*n +1

{-Exercise 2: Folding with trees
Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
Binary_tree
a binary tree is the length of a path from the root to the deepest
node. For example, the height of a tree with a single node is 0; the
height of a tree with three nodes, whose root has two children, is 1;
and so on. A binary tree is balanced if the height of its left and right
subtrees differ by no more than 1, and its left and right subtrees are
also balanced.
For this exercise, write a function
cis 194: homework 4 2
foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using
foldr.-}

data Tree a = Leaf
 | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node h t_l b t_r)
    | height t_l < height t_r = let newTree = insert a t_l in
         Node (1 + max (height newTree) (height t_r)) newTree b t_r
    | otherwise =let newTree = insert a t_r in
         Node (1 + max (height t_l) (height newTree)) t_l b newTree

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h


{-Exercise 3: More folds!
1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains.-}

xor :: [Bool] -> Bool
xor = foldr (\ x y -> (x && not y) || (y && not x) ) False


{-Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.-}

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y)  []

{-(Optional) Implement foldl using foldr. That is, complete the
definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.-}

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f  = foldr (flip f)

{-Exercise 4: Finding primes
Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
of_Sundaram
ing function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.
sieveSundaram :: Integer -> [Integer] -}
--sieveSundaram :: Integer -> [Integer]
--sieveSundaram n = [x | x <-[1..j], i<-[1..j] , not (x == i+j+2*i*j)] 
{-
[((x,i),j) | (x,i) <- [(x,i)| x<-[1..n], i<-[1..j]], j <[1..j]
-}

createIJlist :: (Num b, Enum b, Ord b) => b -> [(b, b)]
createIJlist n  = [(i,j) | i<-[1..n], j<-[1..n], i <=j, i+j + (2*i*j) <= n ]

sumIJlist :: (Num b, Enum b, Ord b) => [(b,b)] -> [b]
sumIJlist = map (\(i,j)-> i + j + (2*i*j))

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+1).(*2)) $ [1..n] \\ sumIJlist (createIJlist n)