module JoinList where
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
 | Single m a
 | Append m (JoinList m a) (JoinList m a)
 deriving (Eq,Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (tag a <> tag b) a b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append m l r)
    | i < 0 || i >= sizeM = Nothing
    | i < sizeL = indexJ i l
    | otherwise = indexJ (i - sizeL) r
    where
        sizeM = getSize $ size m
        sizeL = getSize . size $ tag l
indexJ _ _ = Nothing

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i t@(Single _ _)
    | i <= 0 = t
    | otherwise = Empty
dropJ i t@(Append m l r)
    | i >= sizeM = Empty
    | i < 0 = t
    | i <sizeL = dropJ i l +++ r
    | otherwise = dropJ (i - sizeL) r
    where
        sizeM = getSize $ size m
        sizeL = getSize . size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i t@(Single _ _)
    | i <= 0 = Empty
    | otherwise = t
takeJ i t@(Append m l r)
    | i <= 0 = Empty
    | i >= sizeM = t
    | i <= sizeL = takeJ i l
    | otherwise = l +++ takeJ (i-sizeL) r
    where
        sizeM = getSize $ size m
        sizeL = getSize . size $ tag l


scoreLine :: String -> JoinList Score String
scoreLine string = Single (scoreString string) string

instance Buffer (JoinList (Score, Size) String) where
    line = indexJ
    numLines = getSize . size . tag
    value = getScore. fst . tag
    toString = unlines . jlToList

    fromString s = buildBalanced (map (\x -> Single (scoreString x, 1) x) (lines s))
      where
        buildBalanced [] = Empty
        buildBalanced [x] = x
        buildBalanced xs = let (l, r) = splitAt (length xs `div` 2) xs
                           in buildBalanced l +++ buildBalanced r

    replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
    replaceLine i s t@(Append m l r)
     | i < 0 || i >= sizeM = t
     | i < sizeL = replaceLine i s l +++ r
     | otherwise = l +++ replaceLine (i-sizeL) s r
     where
        sizeM = getSize $ size m
        sizeL = getSize . size $ tag l

    replaceLine 0 s _ = Single (scoreString s, 1) s

    replaceLine _ _ x = x
   