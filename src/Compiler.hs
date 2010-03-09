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

isTBaseDef (TypeBaseDef _ _ _ _) = True
isTBaseDef _ = False

{-- TypeDataDef Ident Kind TypedFunc (Maybe Type) --}

isTDataDecl (TypeDataDef _ _ _ _) = True
isTDataDecl _ = False

isTDataAlias (TypeDataDef _ _ _ a) = isJust a

getTDataAlias (TypeDataDef _ _ _ (Just a)) = a

isTSemantic (TypeSemanticDef _ _ _ _) = True
isTSemantic _ = False

getTBaseConstStr (TypeBaseDef _ _ (Just (BaseConst _ _ s)) _) = s

getTBaseDefString (TypeBaseDef _ _ _ s) = s

getTSemanticString (TypeSemanticDef _ _ _ s) = s

getTSemanticConst (TypeSemanticDef _ _ c _) = c

isTypeCon (TypedTypeConst _ _) = True
isTypeCon _ = False

isTBaseFunc (TypedBaseFunc _ _ _) = True
isTBaseFunc _ = False

getFuncType (TypedTypeConst _ t) = t
getFuncType (TypedFuncBind _ t _ _) = t

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

lookupFuncExists p (_,fs) = not $ null $ filter (\x -> getFuncName x == p) fs


isPattMatch (TypeParenPattern p _) = isPattMatch p
isPattMatch (TypeAppPattern _ _ _) = True
isPattMatch _ = False	    

returnType (TypeFunc t t2) = returnType t2
returnType (TypeParen t) = returnType t
returnType t = t

getTypeIdent (TypeCon c) = c
getTypeIdent (TypeVar i) = i

getFuncPattern (TypedFuncBind _ _ ps _) = ps

getFuncExpr (TypedFuncBind _ _ _ e) = e

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
  body <- compileBody i ((concat binds) ++ b) e
  return (pre ++ " " ++ protoType ++ " " ++ post ++ "{\n" ++ body ++ "\n}")

compileBody i e binds = do
  (src, retBind, bs) <- compileExpr i e binds
  src' <- return $ src ++ ["return " ++ retBind ++ ";"]
  return $ intercalate "\n" src'

compileExpr :: ([TypeDef], [TypedFunc])
	    -> [(Ident, [Char])]
	    -> TypeExp
	    -> StateT CState CompilerM ([String], [Char], [()])

compileExpr i bind (TypeLiteralExp s _) = return ([], (getLiteralStr s), [])

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
  let retType = getTExpType t1
  rStr <- getExpTypeStr i retType
  v <- freshVar
  (asrc, abind, _) <- compileExpr i bind t1
  let nsrc = [rStr ++ " " ++ v ++ " = " ++ abind ++ ";"]
  (bsrc, bbind, _) <- compileExpr i ((ident,v):bind) t2
  return (asrc ++ nsrc ++ bsrc, bbind, [])

compileFuncApp i bind expr = do
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  if isTBaseFunc funcD
    then compileBaseFuncApp i bind expr
    else compileRegFuncApp i bind expr

compileRegFuncApp i bind expr = do
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  let args = getExpFuncArgs expr
      patts = getFuncPattern funcD
  cArgs <- mapM (compileExpr i bind) args
  let abinds = map (\(_,b,_) -> b) cArgs
      asrc = concat $ map (\(s,_,_) -> s) cArgs 
      zap = zip abinds patts
  nbinds <- concat <$> mapM (\(a,p) -> bindParameter i p a) zap
  let fbinds = nbinds ++ bind
  (csrc, cbind, _) <- compileExpr i fbinds (getFuncExpr funcD)
  return (asrc ++ csrc, cbind, [])

bindParameter i (TypeIdentPattern ident _) var = do
  return [(IdentVar ident, var)]

bindParameter i (TypeParenPattern p _) var = bindParameter i p var

bindParameter i p@(TypeAppPattern _ _ _)  var = do
  let (s,_) = getPatternCon 0 p
  funcD <- lookupFunc (IdentCon s) i
  let retType = getTPattType p
  funcT <- lookupType (getTypeCon retType) i
  bindParameterApp' funcT
  where bindParameterApp' funcT
	  | isTDataDecl funcT && isTDataAlias funcT = bindParameter i (head $ getPattVars p) var
	  | isTSemantic funcT = bindParameter i (head $ getPattVars p) var
	  | otherwise = fail "bind not implemented"

getFuncTypeParams (TypeFunc t d) = t:(getTypeParams d)
getFuncTypeParams (TypeParen p) = getTypeParams p
getFuncTypeParams _ = []

compileBaseFuncApp i bind expr = do
  let args = getExpFuncArgs expr
  cArgs <- mapM (compileExpr i bind) args
  let aBinds = map (\(_,b,_) -> b) cArgs
      aSrc = concat $ map (\(s,_,_) -> s) cArgs
      func = getExpFunc expr
  funcD <- lookupFunc func i
  let bStr = doTemplateReplace aBinds (getTBaseFuncStr funcD)
  return (aSrc, bStr, [])

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
	  | isTDataDecl aD = getExpTypeStrData aD
	  | isTSemantic aD = getExpTypeStrSemantic aD
	  | otherwise = fail $ "not yet done getExpTypeStr: " ++ (show aD)

	getExpTypeStrData aD = do
	  if isTDataAlias aD
	    then getExpTypeStr i (getTDataAlias aD)
	    else fail "durf"

	getExpTypeStrSemantic aD = getExpTypeStr i (head $ getTypeParams t)
      

compileTypeCon i bind expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  compileTypeCon' aD
  where compileTypeCon' aD
	  | isTSemantic aD = compileTSemantic i bind expr
	  | isTBaseDef aD = compileTBaseDef i bind expr
	  | isTDataDecl aD = compileTDataDecl i bind expr
	  | otherwise = fail "not implemented compileTypeCon"

compileTSemantic i bind expr = do
  args <- return $ getExpFuncArgs expr
  compileExpr i bind (head args)

compileTBaseDef i bind expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  let args = getExpFuncArgs expr
  cArgs <- mapM (compileExpr i bind) args
  let aBinds = map (\(_,b,_) -> b) cArgs
      aSrc = concat $ map (\(s,_,_) -> s) cArgs
      bStr = doTemplateReplace aBinds (getTBaseConstStr aD)
  return (aSrc, bStr, [])

compileTDataDecl i bind expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  if isTDataAlias aD
    then compileExpr i bind (head $ getExpFuncArgs expr)
    else fail "slsdsflkj"

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
	  | isTDataDecl conT = generateDataDeclParam i p

generateDataDeclParam i p = do
  con <- return $ getTypeCon $ getTPattType p
  conT <- lookupType con i
  if (isTDataAlias conT)
    then generateFuncParam i (makeNewPatt p (getTDataAlias conT))
    else fail "not implemented - generateDataDeclParam"
  where makeNewPatt (TypeIdentPattern i _) t = TypeIdentPattern i t
	makeNewPatt (TypeAppPattern _ b _) t = b
	makeNewPatt (TypeParenPattern p _) t = makeNewPatt p t

generateSemanticParam i p = do
  ty <- return $ getTypeCon $ getTPattType p
  tyDecl <- lookupType ty i
  tvars <- return $ getTypeParams $ getTPattType p 
  v <- freshVar
  varTD <- lookupType (getTypeIdent (head tvars)) i
  bStr <- return $ getTBaseDefString varTD
  pStr <- return $ getTSemanticString tyDecl
  str <- return $ bStr ++ " " ++ v ++ " : " ++ pStr 
  binds <- bindParameter i p v
  return (binds, str)

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
  isDefined <- return $ lookupFuncExists ident i
  ide <- return $ (drop 6 $ getIdentStr ident)
  if lookupFuncExists (IdentVar ide) i
    then do (TypedFuncBind _ _ _ expr) <- lookupFunc (IdentVar ide) i
	    (_, cStr, _) <- compileExpr i [] expr
	    str <- return $ bStr ++ " " ++ ide ++ " = " ++ cStr ++ ";"
	    return ((IdentVar ide, ide),[str])
    else do str <- return $ bStr ++ " " ++ ide ++ ";"
	    return ((IdentVar ide, ide),[str])

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
  let src = psrc ++ [""] ++ [cf]
      output = intercalate "\n" src
  return output

compile a = do 
  (a, _) <- runStateT (compile' a) (CState 0)
  return a