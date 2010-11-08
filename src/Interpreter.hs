{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Interpreter
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

module Interpreter (
    eval, evalList
) where

import Parser.AST
import Text.ParserCombinators.Parsec
import Control.Monad.Error

--
-- Error Type
--

data EvalError = ErrDefault String
                 deriving (Show)

instance Error EvalError where
    noMsg  = ErrDefault "ERROR"
    strMsg = ErrDefault

type ThrowsError = Either EvalError

--
-- Eval function
--

evalList :: [Expression SourcePos] -> ThrowsError [Expression SourcePos]
evalList a = mapM eval a

eval :: Expression SourcePos -> ThrowsError (Expression SourcePos)
eval v@(ExprLiteral _ _) = return v
eval v@(ExprVariable _ _) = return v
eval v@(ExprList [ExprVariable "quote" _, d] _) = return d
eval v@(ExprList (head:tail) _) = do args <- mapM eval tail
                                     apply head args
eval _ = throwError $ ErrDefault "Can't eval"

--
-- Apply function
--

apply :: Expression SourcePos -> [Expression SourcePos] -> ThrowsError (Expression SourcePos)
apply (ExprVariable func pos) args = maybe err (\f -> f pos args) $ lookup func primOps
                  where err = (throwError $ ErrDefault "No such function")
apply _ _ = throwError $ ErrDefault "Can't apply"


getSourcePos :: Expression SourcePos -> SourcePos
getSourcePos (ExprVariable _ a) = a
getSourcePos (ExprLiteral _ a)  = a
getSourcePos (ExprList _ a)     = a
getSourcePos (ExprDotList _ a)  = a

type PrimitiveOp = SourcePos -> [Expression SourcePos] -> ThrowsError (Expression SourcePos)

--
-- Numeric operations
--

addNums :: NumToken -> NumToken -> NumToken
addNums (NumReal r) (NumReal s) = NumReal $ opOverReals (+) r s

subNums :: NumToken -> NumToken -> NumToken
subNums (NumReal r) (NumReal s) = NumReal $ opOverReals (-) r s

opOverReals :: (forall a. Num a => (a -> a -> a)) -> NumRealRep -> NumRealRep -> NumRealRep
opOverReals op (NumRealInteger a) (NumRealInteger b) = NumRealInteger (a `op` b)
opOverReals op a b = NumRealDecimal ((realToFloat a) `op` (realToFloat b))

realToFloat :: NumRealRep -> Float
realToFloat (NumRealDecimal d) = d
realToFloat (NumRealInteger i) = fromInteger i

numZero :: NumToken
numZero = NumReal (NumRealInteger 0)

numOne :: NumToken
numOne = NumReal (NumRealInteger 1)

toNum :: Expression SourcePos -> ThrowsError NumToken
toNum (ExprLiteral (LiteralNum n) _) = return n
toNum _ = throwError $ ErrDefault "Not a number"

addition :: PrimitiveOp
addition pos args = do args' <- mapM toNum args
                       return $ ExprLiteral (LiteralNum $ foldl addNums numZero args') pos

subtraction :: PrimitiveOp
subtraction pos [a] = do arg <- toNum a
                         return $ ExprLiteral (LiteralNum $ numZero `subNums` arg) pos
subtraction pos (head:tail) = do h <- toNum head
                                 t <- mapM toNum tail
                                 return $ ExprLiteral (LiteralNum $ foldl subNums h t) pos

primOps :: [(String, PrimitiveOp)]
primOps = [("+", addition),
           ("-", subtraction)]

