{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-|
Module      : Assign3.Vector
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Vector (Vec) where

data Nat
  = Zero
  | Succ Nat

data Vec a (n :: Nat) where
  Nil :: Vec a 'Zero
  Cons :: a -> Vec a n -> Vec a ('Succ n)

instance Eq a => Eq (Vec a n) where
  Nil == Nil             = True
  Cons a v == Cons a' v' = a == a' && v == v'

instance Show a => Show (Vec a n) where
  show v = '[' : show' v ++ "]" where
    show' :: Show a => Vec a n -> String
    show' Nil          = ""
    show' (Cons a Nil) = show a
    show' (Cons a v)   = show a ++ ", " ++ show' v



class Vector v where
  toList :: v a n -> [a]
  fromList :: [a] -> v a n

{-| 'Vec' is a 'Vector' with a fixed number of elements
-}
-- instance Vector Vec where
--   toList :: Vec a n -> [a]
--   toList Nil         = []
--   toList (Cons x xs) = x : toList xs


