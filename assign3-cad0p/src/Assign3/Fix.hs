{-# OPTIONS_GHC -fmax-simplifier-iterations=1 #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

{-|
Module      : Assign3.Fix
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Fix (foldr, yFoldr) where

import           Prelude hiding (foldr)

-- | A fixed-point combinator
fix :: (a -> a) -> a
fix f = f (fix f)


{-|
  >>> foldr (||) False [False, True, False]
  >>  True
-}
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = fix
  (\foldr' f z l -> case l of
    []     -> z
    (x:xs) -> f x (foldr' f z xs))

{-|
  Error: Occurs check: cannot construct the infinite type: t0 ~ t0 -> t
  Expected type: t0 -> t
    Actual type: (t0 -> t) -> t
-}
-- y = \f -> (\x -> f (x x)) (\x -> f (x x))

{-
  What is happening here is that x is both a function and a parameter in this case.
  The function `x` would have type a ~ a -> b
  The parameter x` should have type b d
-}


{-| Recursive type -}
data F a = F { unF :: F a -> a }



y :: (a -> a) -> a
y f = (\x -> f (unF x x)) (F (\x -> f (unF x x)))

{-|
  The compiler doesn't like to export 'yFoldr', so cannot test in the tests
  but it works the same way as 'foldr'

  >>> foldr (||) False [False, True, False]
  >>  True
-}
yFoldr :: (a -> b -> b) -> b -> [a] -> b
yFoldr = y
  (\foldr' f z l -> case l of
    []     -> z
    (x:xs) -> f x (foldr' f z xs))
