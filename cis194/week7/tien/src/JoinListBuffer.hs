{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JoinListBuffer where

import JoinList
  ( JoinList(Append, Empty, Single)
  , (+++)
  , dropJ
  , indexJ
  , tag
  , takeJ
  )
import Scrabble
import Sized

import Buffer

-- exercise 4 --
instance Buffer (JoinList (Score, Size) String) where
  toString (Single _ x) = x
  toString (Append _ left right) = toString left ++ toString right
  toString _ = ""
  fromString = foldr ((+++) . formJointList) Empty . lines
    where
      formJointList = (\y -> (Single (scoreString y, Size 1) y))
  line n jl = indexJ n jl
  replaceLine n l jl
    | line n jl == Nothing = jl
    | otherwise =
      takeJ n jl +++ (Single (scoreString l, Size 1) l) +++ dropJ (n + 1) jl
  numLines = getSize . size . tag
  value (Append (Score x, _) _ _) = x
  value (Single (Score x, _) _) = x
  value _ = 0
