module Parser.ParseE
  ( ParseE (..)
  , parseSuccess, getParseResult, getParseError
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

parseSuccess (Ok _) = True
parseSuccess _ = False

getParseResult (Ok a) = a
getParseResult _ = undefined

getParseError (Failed s) = s
getParseError _ = undefined