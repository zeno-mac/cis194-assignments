{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random


main :: IO ()
main = do
  gen <- newStdGen
  let res = evalRand (successProb (Battlefield 100 100)) gen
  print res
------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
{-
The rules of attacking in Risk are as follows.
• There is an attacking army (containing some number of units) and
a defending army (containing some number of units).
• The attacking player may attack with up to three units at a time.
However, they must always leave at least one unit behind. That
is, if they only have three total units in their army they may only
attack with two, and so on.
• The defending player may defend with up to two units (or only
one if that is all they have).
• To determine the outcome of a single battle, the attacking and
defending players each roll one six-sided die for every unit they
have attacking or defending. So the attacking player rolls one, two,
or three dice, and the defending player rolls one or two dice.
• The attacking player sorts their dice rolls in descending order. The
defending player does the same.
• The dice are then matched up in pairs, starting with the highest
roll of each player, then the second-highest.
• For each pair, if the attacking player’s roll is higher, then one of
the defending player’s units die. If there is a tie, or the defending
player’s roll is higher, then one of the attacking player’s units die.
-}
type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show


--- Exercise 2
battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield attTotal defTotal) = do 
  let (att,def) = troops bf 
  attDie <- rollDie att
  defDie <- rollDie def
  let attSort = descSort attDie
  let defSort = descSort defDie
  let (attLoss, defLoss) = calcLoss attSort defSort
  return (Battlefield (attTotal + attLoss) (defTotal + defLoss)) 

  

calcLoss :: [DieValue] -> [DieValue] -> (Int, Int)
calcLoss a b = foldr sumTuple (0,0) $ zipWith f a b where 
  sumTuple (a1,a2) (b1,b2) = (a1+b1,a2+b2)
  f a b = if a>b then (0,-1) else (-1,0)

troops :: Battlefield -> (Army,Army)
troops (Battlefield a d) = (att,def) where
  att = min 3 (min0 (a-1))
  def = min 2 d
  min0 x = max x 0

rollDie :: Int -> Rand StdGen [DieValue]
rollDie x = replicateM x die

descSort :: [DieValue] -> [DieValue]
descSort = foldr insert [] where
  insert elem [] = [elem]
  insert elem (x:xs) = if elem > x
    then elem : x :xs
    else x : insert elem xs

--- Exercise 3
invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield att def) 
  |def == 0 || att < 2 =  return bf
  | otherwise =  battle bf >>= invade
  
--- Exercise 4
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do 
  res <- replicateM 1000 (invade bf)
  let win = wins res
  return $ win / 1000.0
  
wins :: [Battlefield] -> Double 
wins = foldr f 0 where 
  f (Battlefield att def) = if def == 0 
    then (1+) 
    else (0+)
