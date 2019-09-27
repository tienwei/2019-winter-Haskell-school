{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m jl1 jl2) = (tag jl1) <> (tag jl2)

-- exercise 1 --
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) jl1 jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- exercise 2 --
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i
  | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- 2.1 --
indexJ' :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ' 1 (Single _ a) = Just a
indexJ' x (Append _ left right)
  | x <= leftSize = indexJ' x left
  | otherwise = indexJ' (x - leftSize) right
  where
    leftSize = getSize . size . tag $left
indexJ' _ _ = Nothing

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ = indexJ' . (+ 1)

-- 2.2 --
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 1 (Append _ (Single _ _) right@(Single _ _)) = right
dropJ x (Append _ left right)
  | x == (getSize . size . tag $ left) = right
  | x < leftSize = (dropJ x left) +++ right
  | otherwise = dropJ (x - leftSize) right
  where
    leftSize = getSize . size . tag $left
dropJ _ _ = Empty
