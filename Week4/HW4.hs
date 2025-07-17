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
fun2 n | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)


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




{-Exercise 3: More folds!
1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains.-} 



{-Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map
function.-}



{-(Optional) Implement foldl using foldr. That is, complete the
definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.-}



{-Exercise 4: Finding primes
Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
of_Sundaram
ing function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.
sieveSundaram :: Integer -> [Integer] -}