module Assign3.Fix (foldr) where

import           Prelude hiding (foldr)

-- | A fixed-point combinator
fix :: (a -> a) -> a
fix f = f (fix f)


foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = fix
  (\foldr' f z l -> case l of
    []     -> z
    (x:xs) -> f x (foldr' f z xs))

