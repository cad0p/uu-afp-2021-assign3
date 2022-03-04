{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}
{-|
Module      : Assign3.Nested
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Nested (Square, Cons) where

type Square      = Square' Nil  -- note that it is eta-reduced
data Square' t a
  = Zero (t (t a))
  | Succ (Square' (Cons t) a)

data Nil a = Nil
data Cons t a
  = Cons a (t a)


{-|
  [[1]]
-}
square1 :: Square Int
square1 = Succ (Zero (Cons (1 `Cons` Nil) Nil))

{-|
  [[1, 0]
  ,[0, 1]]
-}
square2 :: Square Int
square2 = Succ (Succ (Zero (
      1 `Cons` (0 `Cons` Nil) `Cons` (
      0 `Cons` (1 `Cons` Nil) `Cons` Nil))))

{-|
  [[1, 2, 3],
   [4, 5, 6]
  ,[7, 8, 9]]
-}
square3 :: Square Int
square3 = Succ( Succ (Succ (Zero (
      1 `Cons` (2 `Cons` ( 3 `Cons` Nil)) `Cons` (
      4 `Cons` (5 `Cons` ( 6 `Cons` Nil)) `Cons` (
      7 `Cons` (8 `Cons` ( 9 `Cons` Nil)) `Cons` Nil))))))


{-|
  from here: https://joelburget.com/data-newtype-instance-class/
-}
class Shape a where
  fromList2 :: [[b]] ->   a
  toList2   ::   a   -> [[b]]

-- instance Shape (Square a) where
--   toList2
