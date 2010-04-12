module Parser.ParseE
  ( ParseE (..)
  ) where

data ParseE a = Ok a 
              | Failed String
              deriving (Eq,Show)
  
instance Monad ParseE where
  return a = Ok a
  m >>= k = 
    case m of
         Ok a -> k a
         Failed e -> Failed e
  fail a = Failed a