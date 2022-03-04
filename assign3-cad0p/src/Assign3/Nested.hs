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
-- square2 :: Square Int
-- square2 = Succ (Succ ())


{-|
  from here: https://joelburget.com/data-newtype-instance-class/
-}
-- class Shape a where
--   fromList2 :: [[a]]
