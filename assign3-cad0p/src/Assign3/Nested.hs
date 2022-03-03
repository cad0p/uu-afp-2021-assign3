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
data Square' t a = Zero (t (t a)) | Succ (Square' (Cons t) a)

data Nil    a = Nil
data Cons t a = Cons a (t a)

