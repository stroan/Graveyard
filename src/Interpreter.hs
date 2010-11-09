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

--
-- Standard imports
import Control.Monad.Error
import Data.IORef
import Text.ParserCombinators.Parsec

--
-- Internal imports
import Parser

--
-- Interpreter AST type
--
type InterpExpression = Expression (Maybe SourcePos) Env

toInterpExpression :: Expression (Maybe SourcePos) a -> InterpExpression
toInterpExpression (ExprVariable s p) = ExprVariable s p
toInterpExpression (ExprLiteral l p) = ExprLiteral l p
toInterpExpression (ExprList ls p) = ExprList (map toInterpExpression ls) p
toInterpExpression (ExprDotList ls p) = ExprDotList (map toInterpExpression ls) p
toInterpExpression _ = undefined

--
-- Environment Type
--

newtype Env = Env (IORef [(String, IORef InterpExpression)])

emptyEnv :: IO Env
emptyEnv = liftM Env $ newIORef []

isBound :: Env -> String -> IO Bool
isBound (Env env) var = do e <- liftIO $ readIORef env
                           return $ maybe False (const True) $ lookup var e

getVar :: Env -> InterpExpression -> IOThrowsError InterpExpression
getVar en@(Env env) expr
    = do e <- liftIO $ readIORef env
         s <- getExprStr expr
         maybe (throwError $ ErrDefault (getSourcePos expr) "Attempted access of unbound variable")
               (liftIO  . readIORef) (lookup s e)

setVar :: Env -> InterpExpression -> InterpExpression -> IOThrowsError InterpExpression
setVar en@(Env env) name value
  = do e <- liftIO $ readIORef env
       n <- getExprStr name
       maybe (throwError $ ErrDefault (getSourcePos name) "Attempted access of unbound variable")
             (\r -> liftIO $ writeIORef r value)
             (lookup n e)
       return value

defineVar :: Env -> InterpExpression -> InterpExpression -> IOThrowsError InterpExpression
defineVar en@(Env env) name value
   = do n <- getExprStr name
        defined <- liftIO $ isBound en n
        if defined
           then setVar en name value >> return value
           else liftIO $ do
                valueRef <- newIORef value
                e <- readIORef env
                writeIORef env ((n, valueRef):e)
                return value

bindVars :: Env -> [(InterpExpression, InterpExpression)] -> IOThrowsError Env
bindVars en@(Env env) bs
   = do e <- liftIO $ readIORef env
        bs' <- mapM makeEntry bs
        liftM Env $ liftIO $ newIORef (bs' ++ e)
     where makeEntry (x,y) = do n <- getExprStr x
                                r <- liftIO $ newIORef y
                                return (n,r)

getExprStr :: InterpExpression -> IOThrowsError String
getExprStr (ExprVariable v _) = return v
getExprStr v = throwError $
                 ErrDefault (getSourcePos v) "Can only access variables with identifiers"



--
-- Error Type
--

data EvalError = ErrDefault (Maybe SourcePos) String
                 deriving (Show)

instance Error EvalError where
    noMsg  = ErrDefault Nothing "ERROR"
    strMsg = ErrDefault Nothing

type ThrowsError = Either EvalError
type IOThrowsError = ErrorT EvalError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError ParserExpression -> IO String
runIOThrows a = do r <- runErrorT a
                   case r of
                        Left e  -> return $ show e
                        Right e -> return $ show e


--
-- Eval function
--

evalList :: [ParserExpression] -> IOThrowsError [InterpExpression]
evalList a = do env <- liftIO $ emptyEnv
                mapM (($) eval env) (map toInterpExpression a)

eval :: Env -> InterpExpression -> IOThrowsError InterpExpression
eval env v@(ExprLiteral _ _) = return v
eval env v@(ExprVariable s p)
   = do b <- liftIO $ isBound env s
        if not b
           then maybe (throwError $ ErrDefault p ("No known identifier " ++ s))
                      (return  . const (ExprBuiltinFunc s p))
                      (lookup s primOps)
           else getVar env v
eval env v@(ExprList [ExprVariable "quote" _, d] _) = return d
eval env v@(ExprList [ExprVariable "define" p, name, form] _)
   = do f <- eval env form
        defineVar env name f
eval env v@(ExprList [ExprVariable "lambda" _, ExprList args _, ExprList body _] p)
   = return $ ExprFunc args body p env
eval env v@(ExprList (head:tail) _) = do args <- mapM (($) eval env) tail
                                         head' <- eval env head
                                         apply head' args
eval env v = throwError $ ErrDefault (getSourcePos v) "Can't eval"

--
-- Apply function
--

apply :: InterpExpression -> [InterpExpression] -> IOThrowsError InterpExpression
apply (ExprVariable func pos) args
   = maybe err (\f -> liftThrows $ f pos args) $ lookup func primOps
     where err = (throwError $ ErrDefault pos "No such function")
apply (ExprBuiltinFunc func pos) args
   = maybe err (\f -> liftThrows $ f pos args) $ lookup func primOps
     where err = (throwError $ ErrDefault pos "No such function - MASSIVE ERROR")
apply (ExprFunc prms body pos env) args
   = do let argPairs = zip prms args
        e <- bindVars env argPairs
        liftM last $ mapM (eval e) body
apply v _ = throwError $ ErrDefault (getSourcePos v) "Can't apply"


getSourcePos :: InterpExpression -> Maybe SourcePos
getSourcePos (ExprVariable _ a) = a
getSourcePos (ExprLiteral _ a)  = a
getSourcePos (ExprList _ a)     = a
getSourcePos (ExprDotList _ a)  = a
getSourcePos (ExprBuiltinFunc _ a) = a
getSourcePos (ExprFunc _ _ a _) = a

type PrimitiveOp = (Maybe SourcePos) -> [InterpExpression] -> ThrowsError InterpExpression

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

toNum :: InterpExpression -> ThrowsError NumToken
toNum (ExprLiteral (LiteralNum n) p) = return n
toNum e = throwError $ ErrDefault (getSourcePos e) "Not a number"

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

