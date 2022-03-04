{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Assign3.Teletype
Description : Teletype IO
Copyright   : (c) Pier Carlo Cadoppi, 2022
License     : MIT License
Maintainer  : p.c.cadoppi@students.uu.nl
Stability   : experimental
-}

module Assign3.Teletype (Teletype (..), echo, runConsole, mockConsole) where

import           Control.Monad                ((>=>))
import           Control.Monad.State.Class    (MonadState (..))
import           Control.Monad.Trans.RWS.Lazy (RWS)
import qualified Control.Monad.Trans.RWS.Lazy as RWS (ask, modify, runRWS)
import           Prelude                      hiding (getChar, getLine, putChar)
import qualified Prelude                      (getChar, getLine, putChar)

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
-- echo :: Teletype a
-- echo = Get (`Put` echo)


instance Functor Teletype where
  fmap f (End x)   = End (f x)
  fmap f (Get g)   = Get (fmap f . g)
  fmap f (Put c x) = Put c (fmap f x)

getLine :: Teletype String
getLine = g "" where
  g s = Get g' where
    g' c  | c == '\n' = End s
          | otherwise = g (s ++ [c])

instance Applicative Teletype where
  pure = End
  End a <*> tt'    = a <$> tt'
  Get g <*> tt'    = Get (\c -> g c <*> tt')
  Put c tt <*> tt' = Put c (tt <*> tt')

instance Monad Teletype where
  End a >>= f    = f a
  Get g >>= f    = Get (g >=> f)
  Put c tt >>= f = Put c (tt >>= f)


getChar :: Teletype Char
getChar = Get End

putChar :: Char -> Teletype ()
putChar c = Put c (End ())

{-|
  Echo continuously echoes characters

  try it out with `runConsole echo`
-}
echo :: Teletype a
echo = do
  c <- getChar
  putChar c
  echo


instance MonadState Char Teletype where
  get = getChar
  put = putChar

{-
  From my little understanding, the main difference is that this MonadState is not like a normal state
  which is characterized by a simple Char, but can be dynamic and this could cause problems when we try
  to call both get and put at the same time

  On the other hand, it's way more convenient for us to use, as you can see from the very simple implementation.
-}


runConsole :: Teletype a -> IO a
runConsole (End a)    = return a
runConsole (Get g)    = Prelude.getChar >>= runConsole . g
runConsole (Put c tt) = do
  Prelude.putChar c
  runConsole tt


type TeletypeRW = RWS [Char] () [Char]

runRWS :: Teletype a -> TeletypeRW a
runRWS (End a) = return a
runRWS (Get g) = do
  s <- RWS.ask
  runRWS (g (get s)) where
    get (x:xs) = x
    get []     = '\n'
runRWS (Put c tt) = do
  RWS.modify (++ [c])
  runRWS tt


mockConsole :: Teletype a -> [Char] -> (a, [Char])
mockConsole tt s = (a', s') where
  (a', s', _) = RWS.runRWS (runRWS tt) s ""
