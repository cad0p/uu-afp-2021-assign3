{-|
Module      : Assign3.Fix
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Fix (foldr) where

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

