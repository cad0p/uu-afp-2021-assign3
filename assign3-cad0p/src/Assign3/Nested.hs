{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-|
Module      : Assign3.Nested
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Nested (Square, Cons (Cons), Square' (..), Nil (Nil), square1, square2, square3, mapSquare', mapNil) where

type Square = Square' Nil  -- note that it is eta-reduced
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
square1 = Succ (Zero (
      (1 `Cons` Nil) `Cons` Nil))

{-|
  [[1, 0]
  ,[0, 1]]
-}
square2 :: Square Int
square2 = Succ (Succ (Zero
      (1 `Cons` (0 `Cons` Nil) `Cons`
      (0 `Cons` (1 `Cons` Nil) `Cons` Nil))))

{-|
  [[1, 2, 3],
   [4, 5, 6]
  ,[7, 8, 9]]
-}
square3 :: Square Int
square3 = Succ( Succ (Succ (Zero
      (1 `Cons` (2 `Cons` ( 3 `Cons` Nil)) `Cons`
      (4 `Cons` (5 `Cons` ( 6 `Cons` Nil)) `Cons`
      (7 `Cons` (8 `Cons` ( 9 `Cons` Nil)) `Cons` Nil))))))


-- here I tried to create a toList function
{-|
  from here: https://joelburget.com/data-newtype-instance-class/
-}
-- class Shape t a where
--   fromList2 :: [[a]] ->  t a
--   toList2   ::  t a  -> [[a]]

-- instance Shape (Square' t) a where
--   toList2 (Succ a)       = toList2 a
--   toList2 (Zero a)       = toList2' a where

--     toList2' :: t (t a) -> [a] -- c: Cons
--     toList2' c = case c of
--       c == Cons a b -> a : toList2' b
--     -- toList2' Nil = []

-- toList2 :: Square' (Cons t) a -> [[a]]
-- toList2 (Succ a) = toList2 a
-- toList2 (Zero a) = toList2' a where
--   toList2' ::(Nil t, Cons t a) => t a -> [a]
--   toList2' (a `Cons` b) = a : toList2' b


eqNil :: (a -> a -> Bool) -> (Nil a -> Nil a -> Bool)
eqNil _ Nil Nil = True


eqCons :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
       -> (a -> a -> Bool)
       -> (Cons t a -> Cons t a -> Bool)
eqCons eqT eqA (Cons x xs) (Cons y ys) = eqA x y && eqT eqA xs ys

-- The function eqCons does not work because the compiler expects
-- to be able to work with eqA which is polymorphic
-- the compiler tries to match eqT type with eqA type, but fails
-- because the user is free to call this function with two different
-- types

eqSquare' :: (forall b . (b -> b -> Bool) -> (t b -> t b -> Bool))
          -> (a -> a -> Bool)
          -> (Square' t a -> Square' t a -> Bool)
eqSquare' eqT eqA (Zero xs) (Zero ys) = eqT (eqT eqA) xs ys
eqSquare' eqT eqA (Succ xs) (Succ ys) = eqSquare' (eqCons eqT) eqA xs ys
eqSquare' _   _   _         _         = False

{-
  Again, it tries to match together eqT and eqA
  eqT (eqT eqA) resembles the structure of Zero, and it's recursive
  so the type changes every time
-}


eqSquare :: (a -> a -> Bool) -> Square a -> Square a -> Bool
eqSquare = eqSquare' eqNil

instance Eq a => Eq (Square a) where
  (==) = eqSquare (==)


type Mapping f a b = (a -> b) -> f a -> f b

mapNil :: Mapping Nil a b
mapNil _ Nil = Nil


mapCons :: (forall x y . Mapping t x y) -> Mapping (Cons  t) a b
mapCons mapT mapA (Cons x xs) = Cons (mapA x) (mapT mapA xs)


mapSquare' :: (forall x y . Mapping t x y) -> Mapping (Square' t) a b
mapSquare' mapT mapA (Zero xs) = Zero (mapT (mapT mapA) xs)
mapSquare' mapT mapA (Succ xs) = Succ (mapSquare' (mapCons mapT) mapA xs)

mapSquare :: Mapping Square a b
mapSquare = mapSquare' mapNil

instance Functor Square where
  fmap = mapSquare


{-|
  Here it says that it expects a type synonym to only have 1 argument
-}
