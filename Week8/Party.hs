module Week8.Party where
import Week8.Employee
import Data.Tree
import System.IO  


--- Exercise 1
glCons :: Employee -> GuestList -> GuestList
glCons emp (GL list fun) = GL (emp:list) (fun + empFun emp)

instance Semigroup GuestList where
    (<>) (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1+f2)
instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
    | f1 < f2 = gl2
    | otherwise = gl1

--- Exercise 2
treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f base (Node a []) = f a []
treeFold f base (Node a list) = f a (map (treeFold f base) list)


--- Exercise 3 
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss list = (glCons boss bestWithoutSubBoss, bestWithSubBoss) where
    listWithSubBoss = map fst list
    bestWithSubBoss = maxFunGL listWithSubBoss
    listWithoutSubBoss = map snd list
    bestWithoutSubBoss = maxFunGL listWithoutSubBoss

maxFunGL :: [GuestList] -> GuestList
maxFunGL = foldr moreFun mempty


--- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry moreFun tuple where
    tuple = treeFold nextLevel (mempty,mempty) tree

--- Exercise 5

parseCompany :: String -> Either String (Tree Employee)
parseCompany contents =
  case reads contents of
    [(tree, "")] -> Right tree
    _            -> Left "Failed to parse input."

main :: IO ()
main = do
    contents <- readFile "Week8/company.txt"
    case parseCompany contents of
        Right tree -> print (maxFun tree)
        Left err   -> putStrLn err