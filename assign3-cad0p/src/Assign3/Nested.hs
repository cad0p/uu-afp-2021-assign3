{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
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

instance Show a => Show (Nil a) where
  show a = ""

instance (Show a) => Show (Cons Nil a) where
  show (Cons a Nil) = show a

instance (Show a, Show (Square a)) => Show (Cons Square a) where
  show (Cons a (Zero Nil))                = show a ++ "1\n"
  show (Cons a (Succ(Zero (Cons b Nil)))) = show a ++ "2\n"
  show (Cons a (Succ(Succ b)))            = show a ++ "3\n"


instance Show a => (Show (Square' (Cons Nil) a)) where
  show (Zero (Cons x xs))  = show x
  show (Succ (Zero (Nil))) = ""


instance Show a => Show (Square a) where
  show (Zero Nil) = ""
  show (Succ a)   = show a ++ "\n"



{-|
  from here: https://joelburget.com/data-newtype-instance-class/
-}
-- class Shape a where
--   fromList2 :: [[a]]
