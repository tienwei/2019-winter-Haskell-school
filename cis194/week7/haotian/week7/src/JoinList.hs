module JoinList where
import Scrabble

import Data.Monoid
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- Readings:
-- (<>) :: Monoid m => m -> m -> m
-- (<>) = mappend
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l1 l2 = Append (tag l1 <> tag l2) l1 l2



-- task 2

-- given
(!!?) :: [a] -> Int -> Maybe a
[]     !!?      _    = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


-- implement indexJ functions
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i jl = jlToList jl !!? i


indexJ':: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' index (Single _ a)
  | index == 0 = Just a
  | otherwise  = Nothing
indexJ' index (Append m l1 l2) 
  | index < sizeL  = indexJ' index l1
  | index < sizeM m   = indexJ' (index - sizeL) l2
  | otherwise = Nothing
  where 
    sizeM = getSize . size
    sizeL = sizeM . tag $ l1 

--task 2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n l1@(Single _ _)
  | n <= 0 = l1
dropJ n l@(Append m l1 l2)
  | n >= sizeM m = Empty
  | n >= sizeL = dropJ (n - sizeL) l2
  | n > 0 = dropJ n l1 +++ l2
  | otherwise  = l
  where 
    sizeM = getSize . size
    sizeL = sizeM . tag $ l1 
dropJ _ _ = Empty

--task 2 takeJ
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n l1@(Single _ _)
  | n > 0 = l1
takeJ n l@(Append m l1 l2)
  | n >= sizeM m = l
  | n >= sizeL = l1 +++ takeJ (n - sizeL) l2
  | n > 0 = takeJ n l1
  where
    sizeM = getSize . size
    sizeL = sizeM . tag $ l1 
takeJ _ _ = Empty


--task 3
scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str
