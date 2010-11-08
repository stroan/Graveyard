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

data NumBases = Base10
data NumSign = NumPos | NumNeg

data NumRealRep = NumRealInteger Integer
                | NumRealDecimal Float

data NumToken = NumReal NumRealRep

data Expression a = ExprVariable String a
                  | ExprLiteral Literal a
                  | ExprList [Expression a] a
                  | ExprDotList [Expression a] a

data Literal = LiteralBool Bool
             | LiteralString String
             | LiteralChar Char
             | LiteralNum NumToken

instance Show NumRealRep where
    show (NumRealInteger i) = show i
    show (NumRealDecimal f) = show f

instance Show NumToken where
    show (NumReal r) = show r

instance Show (Expression a) where
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

instance Show Literal where
    show (LiteralBool True) = "#t"
    show (LiteralBool False) = "#f"
    show (LiteralString s) = show s
    show (LiteralChar c) = "#\\" ++ [c]
    show (LiteralNum n) = show n
