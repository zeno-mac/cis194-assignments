module Week1.Hanoi where

{-Exercise 5 - Tower of Hanoi-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a,b)]
hanoi x a b c = hanoi (x-1) a c b ++ [(a,b)] ++ hanoi (x-1) c b a

