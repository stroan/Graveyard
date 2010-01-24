module Types
  ( Token (..)
  , ParseE (..)
  , EffectModule (..)
  , TopLevelDecl (..)
  , Ident (..)
  , Exp (..)
  , Pattern (..)
  , Literal (..)
  , Type (..)
  , Constructor (..)
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
           deriving (Eq,Show)

{-- -- -- -- -- -- -- -- -- -- -- -- --
Types defining the AST
-- -- -- -- -- -- -- -- -- -- -- -- -- --}
data EffectModule = EffectModule [TopLevelDecl]
                    deriving (Show, Eq)

data TopLevelDecl = DataDecl Ident [Ident] Constructor 
                  | FuncBindDecl Ident [Ident] Exp
                  | FuncTypeDecl Ident Type
                    deriving (Show, Eq)

data Ident = Ident String
            deriving (Show, Eq)

data Exp = LiteralExp Literal
         | IdentExp String
         | ConsExp String
         | AppExp Exp Exp
         | ParenExp Exp
         | TupleExp [Exp]
           deriving (Show, Eq)

data Literal = LiteralInt String
             | LiteralReal String
             | LiteralString String
             deriving (Show, Eq)
           
data Pattern = Pattern
               deriving (Show, Eq)

data Type = TypeCon String
          | TypeVar String
          | TypeFunc Type Type
          | TypeParen Type
          | TypeApp Type Type
          deriving (Show, Eq)

data Constructor = Constructor Ident [Type]
                 deriving (Show, Eq)

{--
Monad for use in parsing, and contains resulting value.
--}
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
