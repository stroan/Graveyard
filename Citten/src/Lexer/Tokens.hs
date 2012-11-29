module Lexer.Tokens
  ( Token (..)
  , Lexeme (..)
  ) where

{-- -- -- -- -- -- -- -- -- -- -- -- --
Type defining the tokens outputted by the lexer.
-- -- -- -- -- -- -- -- -- -- -- -- -- --}
data Token = TokBuiltin String
           | TokEquals
           | TokTypeSpec
           | TokRArrow
           | TokOpenParen
           | TokCloseParen
           | TokComma
           | TokColon
           | TokStringLit String
           | TokIntLit String
           | TokRealLit String
           | TokConId String
           | TokIdentId String
           | TokError
           | TokEOF
           deriving (Eq,Show)

data Lexeme = Lexeme Int Int Token
              deriving (Show, Eq)