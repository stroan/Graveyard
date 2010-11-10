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
import Data.List
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
                      (lookup s builtinFuncs)
           else getVar env v
eval env v@(ExprList [] _) = return v
eval env v@(ExprList [ExprVariable "eval" _, l] p)
   =  do l' <- eval env l
         case l' of
            ExprList m _ -> do a <- mapM (eval env) m
                               return $ last a
            otherwise    ->  throwError $ ErrDefault p "Can only eval list"
eval env v@(ExprList [ExprVariable "quote" _, d] _) = return d
eval env v@(ExprList [ExprVariable "define" p, name, form] _)
   = do f <- eval env form
        defineVar env name f
eval env v@(ExprList [ExprVariable "if" _, a, b, c] _)
   = do cond <- eval env a
        cond' <- liftThrows $ toBool cond
        if cond'
           then eval env b
           else eval env c
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
   = maybe err (\f -> f pos args) $ lookup func builtinFuncs
     where err = (throwError $ ErrDefault pos "No such function")
apply (ExprBuiltinFunc func pos) args
   = maybe err (\f -> f pos args) $ lookup func builtinFuncs
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
type IOPrimitiveOp = (Maybe SourcePos) -> [InterpExpression] -> IOThrowsError InterpExpression

--
-- Numeric operations
--

addNums :: NumToken -> NumToken -> NumToken
addNums (NumReal r) (NumReal s) = NumReal $ opOverReals (+) r s

subNums :: NumToken -> NumToken -> NumToken
subNums (NumReal r) (NumReal s) = NumReal $ opOverReals (-) r s

multNums :: NumToken -> NumToken -> NumToken
multNums (NumReal r) (NumReal s) = NumReal $ opOverReals (*) r s

divNums :: NumToken -> NumToken -> NumToken
divNums (NumReal r) (NumReal s) = NumReal $ divReals r s

cmpNums :: NumToken -> NumToken -> Ordering
cmpNums (NumReal r) (NumReal s) = cmpReals r s

cmpReals :: NumRealRep -> NumRealRep -> Ordering
cmpReals (NumRealInteger a) (NumRealInteger b) = a `compare` b
cmpReals a b = (realToFloat a) `compare` (realToFloat b)

divReals :: NumRealRep -> NumRealRep -> NumRealRep
divReals (NumRealInteger a) (NumRealInteger b) = NumRealInteger (a `div` b)
divReals a b = NumRealDecimal ((realToFloat a) / (realToFloat b))

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

--
-- String operations
--

toString :: InterpExpression -> ThrowsError String
toString (ExprLiteral (LiteralString n) p) = return n
toString e = return $ show e

concatStr :: PrimitiveOp
concatStr p args = do as <- mapM toString args
                      let astr = foldr (++) "" as
                      return $ ExprLiteral (LiteralString $ astr) p

--
-- Boolean operations
--

toBool :: InterpExpression -> ThrowsError Bool
toBool (ExprLiteral (LiteralBool n) p) = return n
toBool e = throwError $ ErrDefault (getSourcePos e) "Not a boolean"

notBool :: PrimitiveOp
notBool pos [e] = do b <- toBool e
                     return $ ExprLiteral (LiteralBool (not b)) pos
notBool pos _ = throwError $ ErrDefault pos "not takes exactly one operands"

orBool :: PrimitiveOp
orBool pos args@[_,_] = do [a, b] <- mapM toBool args
                           return $ ExprLiteral (LiteralBool (a || b)) pos
orBool pos _ = throwError $ ErrDefault pos "== takes exactly two operands"

andBool :: PrimitiveOp
andBool pos args@[_,_] = do [a, b] <- mapM toBool args
                            return $ ExprLiteral (LiteralBool (a && b)) pos
andBool pos _ = throwError $ ErrDefault pos "== takes exactly two operands"

--
-- Comparison operations
--
exprEq :: PrimitiveOp
exprEq pos [ExprLiteral (LiteralNum a) _, ExprLiteral (LiteralNum b) _]
  = return $ ExprLiteral (LiteralBool $ (cmpNums a b) == EQ) pos
exprEq pos [a,b] = return $ ExprLiteral (LiteralBool $ a == b) pos
exprEq pos _ = throwError $ ErrDefault pos "== takes exactly two operands"

greaterThan :: PrimitiveOp
greaterThan pos args@[_,_] = do [a, b] <- mapM toNum args
                                let gt = cmpNums a b == GT
                                return $ ExprLiteral (LiteralBool gt) pos
greaterThan pos _ = throwError $ ErrDefault pos "> takes exactly two operands"


lessThan :: PrimitiveOp
lessThan pos args@[_,_] = do [a, b] <- mapM toNum args
                             let gt = cmpNums a b == LT
                             return $ ExprLiteral (LiteralBool gt) pos
lessThan pos _ = throwError $ ErrDefault pos "< takes exactly two operators"

--
-- List operations
--

cons :: PrimitiveOp
cons pos [a,(ExprList l _)] = return $ ExprList (a:l) pos
cons pos [a,(ExprDotList l _)] = return $ ExprDotList (a:l) pos
cons pos [a,b] = return $ ExprDotList [a,b] pos
cons pos _ = throwError $ ErrDefault pos "cons takes exactly two operators"

car :: PrimitiveOp
car pos [ExprList (l:_) _] = return l
car pos [ExprList [] _] = throwError $ ErrDefault pos "No first element"
car pos [ExprDotList (l:_) _] = return l
car pos [ExprDotList [] _] = throwError $ ErrDefault pos "No first element"
car pos _ = throwError $ ErrDefault pos "car takes exactly one operator"

cdr :: PrimitiveOp
cdr pos [ExprList (_:l) _] = return $ ExprList l pos
cdr pos [ExprList [] _] = throwError $ ErrDefault pos "No second element"
cdr pos [ExprDotList (_:l) _] = return $ ExprDotList l pos
cdr pos [ExprDotList [] _] = throwError $ ErrDefault pos "No second element"
cdr pos _ = throwError $ ErrDefault pos "cdr takes exactly one operator"


--
-- Primitive Ops
--

addition :: PrimitiveOp
addition pos args = do args' <- mapM toNum args
                       return $ ExprLiteral (LiteralNum $ foldl addNums numZero args') pos

subtraction :: PrimitiveOp
subtraction pos [a] = do arg <- toNum a
                         return $ ExprLiteral (LiteralNum $ numZero `subNums` arg) pos
subtraction pos (head:tail) = do h <- toNum head
                                 t <- mapM toNum tail
                                 return $ ExprLiteral (LiteralNum $ foldl subNums h t) pos

multiply :: PrimitiveOp
multiply pos args = do args' <- mapM toNum args
                       return $ ExprLiteral (LiteralNum $ foldl multNums numOne args') pos

division :: PrimitiveOp
division pos args@([a,b]) = do ([a',b']) <- mapM toNum args
                               return $ ExprLiteral (LiteralNum $ divNums a' b') pos
division pos _ = throwError $ ErrDefault pos "/ takes exactly two operands"

primOps :: [(String, PrimitiveOp)]
primOps = [("+", addition),
           ("-", subtraction),
           ("*", multiply),
           ("/", division),
           ("not", notBool),
           ("==", exprEq),
           (">", greaterThan),
           ("<", lessThan),
           ("and", andBool),
           ("or", orBool),
           ("cons", cons),
           ("car", car),
           ("cdr", cdr),
           ("concat", concatStr)]

--
-- IO Ops
--

printOp :: IOPrimitiveOp
printOp p as = do a <- liftThrows $ mapM toString as
                  liftIO $ putStrLn $ intercalate "," a
                  return $ ExprLiteral (LiteralBool True) p


load :: IOPrimitiveOp
load p [d]
   = do name <- liftThrows $ toString d
        contents <- liftIO $ readFile name
        pDoc <- return $ runParser documentParser () name contents
        case pDoc of
           Left e -> throwError $ ErrDefault p ("Cound not parse file " ++ name ++ "\n" ++ show e)
           Right ast -> return $ ExprList (map toInterpExpression ast) p
load p _ = throwError $ ErrDefault p "load takes exactly two operands"

ioOps :: [(String, IOPrimitiveOp)]
ioOps = [("print", printOp),
         ("load", load)]

builtinFuncs :: [(String, IOPrimitiveOp)]
builtinFuncs = liftedOps ++ ioOps
               where liftedOps = map (\(x,y) -> (x, \a b -> liftThrows $ y a b)) primOps

