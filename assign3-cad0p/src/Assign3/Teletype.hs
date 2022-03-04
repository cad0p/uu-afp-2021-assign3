{-|
Module      : Assign3.Teletype
Description : Teletype IO
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Teletype (Teletype (..)) where

import           Prelude hiding (getLine)
import qualified Prelude (getLine)

data Teletype a
  = End a
  | Get (Char -> Teletype a)
  | Put Char (Teletype a)

instance Show a => Show (Teletype a) where
  show (Get _)    = "Get g"
  show (Put c tt) = "Put " ++ [c] ++ " (" ++ show tt ++ ")"
  show (End a)    = "Return " ++ show a


instance Eq a => Eq (Teletype a) where
  (Get g) == (Get g')        = g 'c' == g' 'c'
  (Put c tt) == (Put c' tt') = c == c' && tt == tt'
  (End a) == (End a')        = a == a'
  _ == _                     = False


{-|
  Echo continuously echoes characters
-}
echo :: Teletype a
echo = Get (`Put` echo)


instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)

getLine :: Teletype String
getLine = g "" where
  g s = Get g' where
    g' c  | c == '\n' = End s
          | otherwise = g (s ++ [c])