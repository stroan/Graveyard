-----------------------------------------------------------------------------
--
-- Module      :  Parser.AST
-- Copyright   :  Stephen Roantree
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser.AST (
    NumBases(..),
    NumSign(..),
    NumRealRep(..),
    NumToken(..),
    Expression(..),
    Literal(..)
) where

import Data.List
import Data.IORef

data NumBases = Base10
data NumSign = NumPos | NumNeg

data NumRealRep = NumRealInteger Integer
                | NumRealDecimal Float
                deriving (Eq)

data NumToken = NumReal NumRealRep
              deriving (Eq)

data Expression a b = ExprVariable String a
                  | ExprLiteral Literal a
                  | ExprList [Expression a b] a
                  | ExprDotList [Expression a b] a
                  | ExprFunc [Expression a b] [Expression a b] a b
                  | ExprBuiltinFunc String a

data Literal = LiteralBool Bool
             | LiteralString String
             | LiteralChar Char
             | LiteralNum NumToken
             deriving (Eq)

instance Eq (Expression a b) where
    (ExprVariable a _) == (ExprVariable b _) = a == b
    (ExprLiteral a _) == (ExprLiteral b _) = a == b
    (ExprList a _) == (ExprList b _) = a == b
    (ExprDotList a _) == (ExprDotList b _) = a == b
    _ == _ = False

instance Show NumRealRep where
    show (NumRealInteger i) = show i
    show (NumRealDecimal f) = show f

instance Show NumToken where
    show (NumReal r) = show r

instance Show (Expression a b) where
    show (ExprVariable s _) = s
    show (ExprLiteral l _) = show l
    show (ExprList l _) = let exprs = map show l
                              str = intercalate " " exprs
                          in "(" ++ str ++ ")"
    show (ExprDotList l _) = let exprs = map show l
                                 str = buildStr exprs
                             in "(" ++ str ++ ")"
                             where buildStr (a:[]) = ". " ++ a
                                   buildStr (a:b) = a ++ " " ++ buildStr b
    show (ExprBuiltinFunc s _) = show $ "<builtin: " ++ s ++ ">"
    show (ExprFunc as bs _ _) = show $ "(lambda (" ++ args ++ ") (" ++ body ++ "))"
                                where args = intercalate " " (map show as)
                                      body = intercalate " " (map show bs)

instance Show Literal where
    show (LiteralBool True) = "#t"
    show (LiteralBool False) = "#f"
    show (LiteralString s) = show s
    show (LiteralChar c) = "#\\" ++ [c]
    show (LiteralNum n) = show n


