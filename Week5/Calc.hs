{-# LANGUAGE FlexibleInstances #-}
module Week5.Calc where
import Week5.ExprT
import Week5.Parser (parseExp)
import Week5.StackVM
import qualified Data.Map as M
{-
Exercise 1
Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
eval :: ExprT -> Integer
-}

eval :: ExprT -> Integer
eval (Week5.ExprT.Lit x) = x
eval (Week5.ExprT.Add x y) = eval x + eval y
eval (Week5.ExprT.Mul x y) = eval x * eval y


{-
Exercise 2
evalStr :: String -> Maybe Integer
-}

evalStr :: String -> Maybe Integer
evalStr s = condEval $ parseExp Week5.ExprT.Lit Week5.ExprT.Add Week5.ExprT.Mul s where
    condEval Nothing = Nothing
    condEval (Just x) = Just $ eval x


{-
Exercise 3
Expr typeclass-}
class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Week5.ExprT.Lit
    mul = Week5.ExprT.Mul
    add = Week5.ExprT.Add


{-
Exercise 4
Make instances of Expr for each of the following types: Integer, Bool, MinMax, Mod7-}

instance Expr Integer where
    lit = id
    add :: Integer -> Integer -> Integer
    add = (+)
    mul = (*)

instance Expr Bool where
    lit x = x > 0
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    lit = MinMax
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)

instance Expr Mod7 where
    lit x = Mod7 (mod x 7)
    add (Mod7 x) (Mod7 y)= Mod7 (mod (x+y) 7)
    mul (Mod7 x) (Mod7 y)= Mod7 (mod (x*y) 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

{-Exercise 5-}
instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [Week5.StackVM.Add]
    mul x y = x ++ y ++ [Week5.StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul

{-Exercise 6-}

class HasVars a where
    var :: String -> a

data VarExprT = Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
           | Var String
  deriving (Show, Eq)

instance Expr VarExprT where
    lit = Week5.Calc.Lit
    mul = Week5.Calc.Mul
    add = Week5.Calc.Add

instance HasVars VarExprT where
    var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit n _ = Just n
    add a b m = case (a m, b m) of
                  (Just n1, Just n2) -> Just (n1 + n2)
                  _ -> Nothing
    mul a b m = case (a m, b m) of
                  (Just n1, Just n2) -> Just (n1 * n2)
                  _ -> Nothing

withVars :: [(String, Integer)]
 -> (M.Map String Integer -> Maybe Integer)
 -> Maybe Integer
withVars vs exp = exp $ M.fromList vs