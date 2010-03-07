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
import Data.String.Utils
import qualified Data.Map as Map
import Types


data CState = CState { cstateID :: Integer }
type CStateM = StateT CState CompilerM

freshVar :: CStateM String
freshVar = StateT (\s -> return (("var" ++ (show (cstateID s))), CState $ (cstateID s)+1))


getFuncName (TypedTypeConst i _) = i
getFuncName (TypedBaseFunc i _ _) = i
getFuncName (TypedFuncBind i _ _ _) = i
getFuncName (TypedParamDecl i _) = i

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

isTBaseFunc (TypedBaseFunc _ _ _) = True
isTBaseFunc _ = False

getFuncType (TypedTypeConst _ t) = t

getTBaseFuncStr (TypedBaseFunc _ _ s) = s

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


isPattMatch (TypeParenPattern p _) = isPattMatch p
isPattMatch (TypeAppPattern _ _ _) = True
isPattMatch _ = False	    

returnType (TypeFunc t t2) = returnType t2
returnType (TypeParen t) = returnType t
returnType t = t

getTypeCon (TypeApp t _) = getTypeCon t
getTypeCon (TypeCon c) = c
getTypeCon (TypeParen t) = getTypeCon t

getTypeIdent (TypeCon c) = c
getTypeIdent (TypeVar i) = i

getTypeParams (TypeApp t d) = (getTypeParams t) ++ [d]
getTypeParams (TypeParen p) = getTypeParams p
getTypeParams _ = []

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

{-------------
Compile function
-------------}

compileFunc i@(ts, fs) (TypedFuncBind ident t ps e) b = do
  retType <- return $ getTExpType e
  params <- mapM (generateFuncParam i) ps
  ps <- return $ map snd params
  binds <- return $ map fst params
  protoType <- return $ (getIdentStr ident) ++ "(" ++ (intercalate "," ps) ++ ")"
  (pre, post) <- generateReturnType i (returnType t)
  body <- compileBody i (binds ++ b) e
  return (pre ++ " " ++ protoType ++ " " ++ post ++ "{\n" ++ body ++ "\n}")

compileBody i e binds = do
  (src, retBind, bs) <- compileExpr i e binds
  src' <- return $ src ++ ["return " ++ retBind ++ ";"]
  return $ intercalate "\n" src'

compileExpr :: ([TypeDef], [TypedFunc])
	    -> [(Ident, [Char])]
	    -> TypeExp
	    -> StateT CState CompilerM ([String], [Char], [()])

compileExpr i bind (TypeParenExp e _) = compileExpr i bind e

compileExpr i bind (TypeIdentExp s t) = do
  b <- lookupBind (IdentVar s) bind
  return ([], b, [])

compileExpr i bind expr@(TypeAppExp t t' _) = do
  func <- return $ getExpFunc t
  funcD <- lookupFunc func i
  if isTypeCon funcD
    then compileTypeCon i bind expr
    else compileFuncApp i bind expr

compileExpr i bind expr@(TypeLetExp ident t1 t2 _) = do
  (asrc, abind, _) <- compileExpr i bind t1
  (bsrc, bbind, _) <- compileExpr i ((ident,abind):bind) t2
  return (asrc ++ bsrc, bbind, [])

compileFuncApp i bind expr = do
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  if isTBaseFunc funcD
    then compileBaseFuncApp i bind expr
    else undefined

compileBaseFuncApp i bind expr = do
  let args = getExpFuncArgs expr
      retType = getTExpType expr
  cArgs <- mapM (compileExpr i bind) args
  let aBinds = map (\(_,b,_) -> b) cArgs
      aSrc = concat $ map (\(s,_,_) -> s) cArgs
      func = getExpFunc expr
  funcD <- lookupFunc func i
  rStr <- getExpTypeStr i retType
  v <- freshVar
  let bStr = doTemplateReplace aBinds (getTBaseFuncStr funcD)
      str = rStr ++ " " ++ v ++ " = " ++ bStr ++ ";"
  return (aSrc ++ [str], v, [])

doTemplateReplace args str =
  let n = map (\x -> "$" ++ show x ++ "$") [1..]
      pairs = zip args n
  in foldl (\s (a, n) -> replace n a s) str pairs

getExpTypeStr i t = do
  a <- return $ getTypeCon t
  aD <- lookupType a i
  getExpTypeStr' aD
  where getExpTypeStr' aD 
	  | isTBaseDef aD = return $ getTBaseDefString aD
	  | otherwise = undefined
      

compileTypeCon i bind expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  if isTSemantic aD
    then compileTSemantic i bind expr
    else fail "not implemented yet lol - compileTypeCon"--}

compileTSemantic i bind expr = do
  args <- return $ getExpFuncArgs expr
  compileExpr i bind (head args)

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
  tvars <- return $ getTypeParams $ getTPattType p 
  v <- freshVar
  varTD <- lookupType (getTypeIdent (head tvars)) i
  bStr <- return $ getTBaseDefString varTD
  pStr <- return $ getTSemanticString tyDecl
  str <- return $ bStr ++ " " ++ v ++ " : " ++ pStr 
  if isPattMatch p
    then return ((getPattIdent $ head $ getPattVars p, v), str)
    else return ((getPattIdent p, v), str)

{----------------------
Compile parameters.
----------------------}

compileParameters i = do
  ps <- return $ getTParamDecls $ snd i
  cps <- mapM (compileParameter i) ps
  binds <- return $ map fst cps
  src <- return $ concat $ map snd cps
  return (binds, src)

compileParameter i (TypedParamDecl ident t) = do
  varTD <- lookupType (getTypeIdent t) i
  bStr <- return $ getTBaseDefString varTD
  str <- return $ bStr ++ " " ++ (getIdentStr ident) ++ ";"
  return ((ident, getIdentStr ident),[str])

getTParamDecls fs = filter isTParamDecl fs

isTParamDecl (TypedParamDecl _ _) = True
isTParamDecl _ = False

{----------------------
 Entry point
----------------------}
  
compile' i = do
  (pbinds, psrc) <- compileParameters i
  vf <- lookupFunc (IdentVar "vertexFragment") i
  cf <- compileFunc i vf pbinds
  let src = psrc ++ [cf]
      output = intercalate "\n" src
  return output

compile a = do 
  (a, _) <- runStateT (compile' a) (CState 0)
  return a