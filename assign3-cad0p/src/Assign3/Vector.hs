{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
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

data Vec a n where
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
  data V v :: * -> Nat -> *
  toList :: V v a n -> [a]
  fromList :: [a] -> V v a n

{-| 'Vec' is a 'Vector' with a fixed number of elements
-}
instance Vector (Vec a n) where
  data V (Vec a n) a n where
    VNil :: V (Vec a 'Zero) a 'Zero
    VCons :: a -> V (Vec a n) a n -> V (Vec a ('Succ n)) a ('Succ n)
  toList VNil         = []
  toList (VCons x xs) = x : toList xs


