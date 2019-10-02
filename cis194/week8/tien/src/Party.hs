{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Employee

-- exercie 1 --
-- 1.1 --
glCons :: Employee -> GuestList -> GuestList
glCons em@(Emp _ emf) (GL empL fun) = GL (em : empL) (fun + emf)

-- 1.2 --
instance Semigroup GuestList where
  (GL empL1 fun1) <> (GL empL2 fun2) = GL (empL1 ++ empL2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

-- 1.3 --
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- exercie 2 --
data Tree a =
  Node
    { rootLabel :: a -- label value
    , subForest :: [Tree a] -- zero or more child trees
    }

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node l []) = f l []
treeFold f (Node l s) = f l $ treeFold f <$> s
