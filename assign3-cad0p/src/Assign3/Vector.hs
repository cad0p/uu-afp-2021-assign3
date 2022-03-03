{-# LANGUAGE GADTs #-}
{-|
Module      : Assign3.Vector
Description : Term-level fixpoints
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Vector (Vec) where

data Zero
data Succ n

data Vec a n where
  Nil :: Vec a Zero
  Cons :: a -> Vec a n -> Vec a (Succ n)


data Foo a
  = Foo
      { a  :: Int
      , a2 :: String
        -- ^ some haddock
      }
  | Bar
      { b :: a
      }
  deriving (Eq, Show)


