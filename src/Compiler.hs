module Compiler 
  ( compile
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Types


data CState = CState { cstateID :: Integer }
type CStateM = StateT CState CompilerM

freshVar :: CStateM String
freshVar = StateT (\s -> return (("var" ++ (show (cstateID s))), CState $ (cstateID s)+1))


getFuncName (TypedTypeConst i _) = i
getFuncName (TypedBaseFunc i _ _) = i
getFuncName (TypedFuncBind i _ _ _) = i

getFuncByName i fs = head (filter (\x -> getFuncName x == i) fs)

{--
data TypeDef = TypeBaseDef Ident Kind BaseConst String
	     | TypeSemanticDef Ident Kind SemanticConst String
	     deriving (Show, Eq)
--}

getTypeName (TypeBaseDef i _ _ _) = i
getTypeName (TypeSemanticDef i _ _ _) = i

isTBaseDef (TypeBaseDef _ _ _ _) = True
isTBaseDef _ = False

isTSemantic (TypeSemanticDef _ _ _ _) = True
isTSemantic _ = False

getTBaseDefString (TypeBaseDef _ _ _ s) = s

getTSemanticString (TypeSemanticDef _ _ _ s) = s

getTSemanticConst (TypeSemanticDef _ _ c _) = c

getIdentStr (IdentVar i) = i
getIdentStr (IdentCon c) = c

isTypeCon (TypedTypeConst _ _) = True
isTypeCon _ = False

getFuncType (TypedTypeConst _ t) = t

{--
data TypedFunc = TypedTypeConst Ident Type
	       | TypedBaseFunc Ident Type String
	       | TypedFuncBind Ident Type [TypePattern] TypeExp
	       deriving (Show, Eq)
--}

lookupType p (ts, _) = do
  a <- return $ filter (\x -> getTypeName x == p) ts
  if (null a)
    then fail $ "Can't find type " ++ show p
    else return $ head a

lookupFunc p (_,fs) = do
  a <- return $ filter (\x -> getFuncName x == p) fs
  if (null a)
    then fail $ "Can't find function " ++ show p
    else return $ head a

lookupBind i binds = do
  a <- return $ lookup i binds
  if (isJust a) 
    then return $ fromJust a
    else fail ("No bind for " ++ getIdentStr i)

compileFunc i@(ts, fs) (TypedFuncBind ident t ps e) = do
  retType <- return $ getTExpType e
  params <- mapM (generateFuncParam i) ps
  ps <- return $ map snd params
  binds <- return $ map fst params
  protoType <- return $ (getIdentStr ident) ++ "(" ++ (intercalate "," ps) ++ ")"
  (pre, post) <- generateReturnType i (returnType t)
  body <- compileBody i e binds
  return (pre ++ " " ++ protoType ++ " " ++ post ++ "{" ++ body ++ "}")

compileBody i e binds = do
  (src, retBind, bs) <- compileExpr i e binds
  src' <- return $ src ++ ["return " ++ retBind ++ ";"]
  return $ intercalate "\n" src'

compileExpr i (TypeIdentExp s t) bind = do
  b <- lookupBind (IdentVar s) bind
  return ([], b, [])

compileExpr i expr@(TypeAppExp t t' _) bind = do
  func <- return $ getExpFunc t
  funcD <- lookupFunc func i
  if isTypeCon funcD
    then compileTypeCon i bind expr
    else fail "not implemented yet lol - compileExpr"

compileTypeCon i bind expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  return ([], show a, [])
  aD <- lookupType a i
  if isTSemantic aD
    then compileTSemantic i bind expr
    else fail "not implemented yet lol - compileTypeCon"--}

compileTSemantic i bind expr = do
  args <- return $ getExpFuncArgs expr
  compileExpr i (head args) bind
  
getExpFuncType (TypeIdentExp _ t) = t
getExpFuncType (TypeConsExp _ t) = t
getExpFuncType (TypeAppExp e _ _) = getExpFuncType e
getExpFuncType (TypeParenExp e _) = getExpFuncType e

getExpFunc (TypeIdentExp i _) = (IdentVar i)
getExpFunc (TypeConsExp c _) = (IdentCon c)
getExpFunc (TypeAppExp e _ _) = getExpFunc e
getExpFunc (TypeParenExp e _) = getExpFunc e

getExpFuncArgs (TypeAppExp t t' _) = getExpFuncArgs t ++ [t']
getExpFuncArgs (TypeParenExp t _) = getExpFuncArgs t
getExpFuncArgs _ = []

generateReturnType i t = do
  con <- return $ getTypeCon t
  conT <- lookupType con i
  generateRet' conT
  where generateRet' conT
	  | isTSemantic conT = generateSemanticReturn i t

generateSemanticReturn i t = do
  ty <- return $ getTypeCon t
  tyDecl <- lookupType ty i
  a <- return $ getParamT t
  varTD <- lookupType (getTypeCon a) i
  if (isTBaseDef varTD)
    then return (getTBaseDefString varTD,": " ++ (getTSemanticString tyDecl))
    else fail "invalid semantic parameter"
  where getParamT (TypeApp _ t) = t

generateFuncParam i p = do
  con <- return $ getTypeCon $ getTPattType p
  conT <- lookupType con i
  generateParam' conT
  where generateParam' conT
	  | isTSemantic conT = generateSemanticParam i p

generateSemanticParam i p = do
  ty <- return $ getTypeCon $ getTPattType p
  tyDecl <- lookupType ty i
  vars <- return $ getPattVars p
  (con, _) <- return $ getPatternCon 0 p
  conF <- lookupFunc (IdentCon con) i
  recordLen <- return $ getRecordLength conF
  if (length vars /= recordLen || recordLen /= 1) 
    then fail "Invalid semantic pattern"
    else do v <- freshVar
	    varT <- return $ getTPattType (head vars)
	    varTD <- lookupType (getTypeCon varT) i
	    if (isTBaseDef varTD)
	      then do i <- return $ getPattIdent (head vars)
		      str <- return $ (getTBaseDefString varTD) ++ " "
			      ++ v ++ " : " ++ (getTSemanticString tyDecl)
		      return ((i, v), str)
	      else fail "invalid semantic parameter"
	    

returnType (TypeFunc t t2) = returnType t2
returnType (TypeParen t) = returnType t
returnType t = t

getTypeCon (TypeApp t _) = getTypeCon t
getTypeCon (TypeCon c) = c
getTypeCon (TypeParen t) = getTypeCon t

getPatternCon d (TypeParenPattern p _) = getPatternCon d p
getPatternCon d (TypeConPattern s t) = (s, d)
getPatternCon d (TypeAppPattern p _ _) = getPatternCon (d+1) p

getPattVars (TypeParenPattern p _) = getPattVars p
getPattVars (TypeAppPattern p t _) = getPattVars p ++ [t]
getPattVars _ = []

getPattIdent (TypeIdentPattern s _) = (IdentVar s)

getRecordLength (TypedTypeConst _ t) = getRecordLength' 0 t
  where getRecordLength' d (TypeFunc t _) = getRecordLength' (d+1) t
	getRecordLength' d (TypeParen t) = getRecordLength' d t
	getRecordLength' d (TypeGen _ t) = getRecordLength' d t
	getRecordLength' d _ = d

compile' i = do
  vf <- lookupFunc (IdentVar "vertexFragment") i
  cf <- compileFunc i vf
  return cf

compile a = do 
  (a, _) <- runStateT (compile' a) (CState 0)
  return a