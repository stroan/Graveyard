module CodeGenerator 
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
import TypeChecker.TypedAST
import CompilerM


data CState = CState { cstateID :: Integer }
type CStateM = StateT CState CompilerM

freshVar :: CStateM String
freshVar = StateT (\s -> return (("var" ++ (show (cstateID s))), CState $ (cstateID s)+1))

getFuncByName i fs = head (filter (\x -> getFuncName x == i) fs)

{--
data TypeDef = TypeBaseDef Ident Kind BaseConst String
	     | TypeSemanticDef Ident Kind SemanticConst String
	     deriving (Show, Eq)
--}

isTBaseDef (TypeBaseDef _ _ _ _ _) = True
isTBaseDef _ = False

{-- TypeDataDef Ident Kind TypedFunc (Maybe Type) --}

isTDataDecl (TypeDataDef _ _ _ _) = True
isTDataDecl _ = False

isTDataAlias (TypeDataDef _ _ _ a) = isJust a

getTDataAlias (TypeDataDef _ _ _ (Just a)) = a

isTSemantic (TypeSemanticDef _ _ _ _) = True
isTSemantic _ = False

getTBaseConstStr (TypeBaseDef _ _ (Just (BaseConst _ _ s)) _ _) = s

getTBaseDefString (TypeBaseDef _ _ _ s _) = s

getTSemanticString (TypeSemanticDef _ _ _ s) = s

getTSemanticConst (TypeSemanticDef _ _ c _) = c

isTypeCon (TypedTypeConst _ _) = True
isTypeCon _ = False

isTBaseFunc (TypedBaseFunc _ _ _) = True
isTBaseFunc _ = False

isTTechniqueDecl (TypeTechniqueDecl _ _) = True
isTTechniqueDecl _ = False

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
getTypeIdent (TypeParen p) = getTypeIdent p

getFuncPattern (TypedFuncBind _ _ ps _) = ps

getFuncExpr (TypedFuncBind _ _ _ e) = e

getPatternCon d (TypeParenPattern p _) = getPatternCon d p
getPatternCon d (TypeConPattern s t) = (s, d)
getPatternCon d (TypeAppPattern p _ _) = getPatternCon (d+1) p

getPattVars (TypeParenPattern p _) = getPattVars p
getPattVars (TypeAppPattern p t _) = getPattVars p ++ [t]
getPattVars _ = []

getPattIdent (TypeParenPattern p _) = getPattIdent p
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



isSideEffect i ident = do
  ty <- lookupType ident i
  return $ isSideEffect' ty
  where isSideEffect' (TypeBaseDef _ _ _ _ s) = s
	isSideEffect' _ = False

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

compileBody i binds e = do
  (src, retBind, bs) <- compileExpr i binds [] e
  se <- isSideEffect i (getTypeCon (getTExpType e))
  if se
    then do return $ intercalate "\n" src
    else do src' <- return $ src ++ ["return " ++ retBind ++ ";"]
	    return $ intercalate "\n" src'

compileExpr :: ([TypeDef], [TypedFunc])
	    -> [(Ident, [Char])]
	    -> [(Ident, ([(Ident, [Char])], [TypePattern], TypeExp))]
	    -> TypeExp
	    -> StateT CState CompilerM ([String], [Char], [()])

compileExpr i bind bexps (TypeLiteralExp s _) = return ([], (getLiteralStr s), [])

compileExpr i bind bexps (TypeParenExp e _) = compileExpr i bind bexps e

compileExpr i bind bexps e@(TypeIdentExp s t) = do
  let bexists = isJust $ lookup (IdentVar s) bind
  if bexists 
     then do b <- lookupBind (IdentVar s) bind
             return ([], b, [])
     else if not $ lookupFuncExists (IdentVar s) i 
             then fail "unknown ident"
             else compileFuncApp i bind bexps e

compileExpr i bind bexps expr@(TypeAppExp t t' _) = do
  func <- return $ getExpFunc t
  if (isJust . (lookup func)) bexps
    then compileLetFuncApp i bind bexps expr
    else do funcD <- lookupFunc func i
	    if isTypeCon funcD
	      then compileTypeCon i bind bexps expr
	      else compileFuncApp i bind bexps expr

compileExpr i bind bexps expr@(TypeLetExp ident [] t1 t2 _) = do
  let retType = getTExpType t1
  rStr <- getExpTypeStr i retType
  v <- freshVar
  (asrc, abind, _) <- compileExpr i bind bexps t1
  (bsrc, bbind, _) <- compileExpr i ((ident,v):bind) bexps t2
  se <- isSideEffect i (getTypeCon retType)
  if se
    then do return (asrc ++ [abind ++ ";"] ++ bsrc, bbind, [])
    else do let nsrc = [rStr ++ " " ++ v ++ " = " ++ abind ++ ";"]
	    return (asrc ++ nsrc ++ bsrc, bbind, [])

compileExpr i bind bexps expr@(TypeLetExp ident ps t1 t2 _) = do
  compileExpr i bind ((ident, (bind, ps, t1)):bexps) t2

compileExpr i bind bexps expr@(TypeIfExp c t f _) = do
  let retType = getTExpType t
  rStr <- getExpTypeStr i retType
  v <- freshVar
  (csrc, cbind, _) <- compileExpr i bind bexps c
  (tsrc, tbind, _) <- compileExpr i bind bexps t
  (fsrc, fbind, _) <- compileExpr i bind bexps f
  se <- isSideEffect i (getTypeCon retType)
  if se
    then return (csrc 
		++ ["if (" ++ cbind ++ ") {"]
		++ tsrc ++ [tbind ++ ";"]
		++ ["}else{"] 
		++ fsrc ++ [fbind ++ ";"]
		++ ["}"], v, [])
    else return (csrc 
		++ [rStr ++ " " ++ v ++ ";"] 
		++ ["if (" ++ cbind ++ ") {"]
		++ tsrc
		++ [v ++ " = " ++ tbind ++ ";}else{"] 
		++ fsrc
		++ [v ++ " = " ++ fbind ++ ";}"], v, [])

compileExpr i bind bexps expr@(TypeLoopExp l w s _) = do
  aVar <- freshVar
  cVar <- freshVar
  (ssrc, sbind, _) <- compileExpr i bind bexps s
  wexp <- makeTestExp
  (wsrc, wbind, _) <- compileExpr i ((IdentVar "a", aVar):(IdentVar "c", cVar):bind) bexps wexp
  lexp <- makeLoopExp
  (lsrc, lbind, _) <- compileExpr i ((IdentVar "a", aVar):(IdentVar "c", cVar):bind) bexps lexp
  let retType = getTExpType expr
  rStr <- getExpTypeStr i retType
  se <- isSideEffect i (getTypeCon retType)
  if se
    then do let src = ssrc ++
		      ["int " ++ cVar ++ " = 0;"] 
		      ++ wsrc ++
		      ["while (" ++ wbind ++ ") {"]
		      ++ lsrc ++
		      ["cVar++;"]
		      ++ wsrc ++
		      ["}"]
	    return (src, aVar, [])
    else do let src = ssrc ++
		      ["int " ++ cVar ++ " = 0;"
		      ,rStr ++ " " ++ aVar ++ " = " ++ sbind ++ ";"] 
		      ++ wsrc ++
		      ["while (" ++ wbind ++ ") {"]
		      ++ lsrc ++
		      [aVar ++ " = " ++ lbind ++ ";"
		      ,cVar ++ "++;"]
		      ++ wsrc ++
		      ["}"]
	    return (src, aVar, [])
  where makeTestExp = do
	  if (isJust . (lookup w)) bexps
             then do let (_, fps, fexp) = fromJust $ lookup w bexps
                         ps = map getTPattType fps
                         ftype = foldr (\x y -> TypeFunc x y) (getTExpType fexp) ps
			 e1 = (TypeAppExp (TypeIdentExp (getIdentStr w) ftype) (TypeIdentExp "a" (ps !! 0)) (getLeft ftype))
	                 e2 = (TypeAppExp e1 (TypeIdentExp "c" (ps !! 1)) (TypeCon (IdentCon "Bool")))
                     return e2
	     else do a <- lookupFunc w i
		     ps <- return (getFuncTypeParams (getFuncType a))
	             let e1 = (TypeAppExp (TypeIdentExp (getIdentStr w) (getFuncType a)) (TypeIdentExp "a" (ps !! 0)) (getLeft (getFuncType a)))
	                 e2 = (TypeAppExp e1 (TypeIdentExp "c" (ps !! 1)) (TypeCon (IdentCon "Bool")))
	             return e2

	makeLoopExp = do
	  if (isJust . (lookup l)) bexps
             then do let (_, fps, fexp) = fromJust $ lookup l bexps
                         ps = map getTPattType fps
                         ftype = foldr (\x y -> TypeFunc x y) (getTExpType fexp) ps
			 e1 = (TypeAppExp (TypeIdentExp (getIdentStr l) ftype) (TypeIdentExp "a" (ps !! 0)) (getLeft ftype))
	                 e2 = (TypeAppExp e1 (TypeIdentExp "c" (ps !! 1)) (getTExpType expr))
                     return e2
             else do a <- lookupFunc l i
	             let ps = getFuncTypeParams (getFuncType a)
	                 e1 = (TypeAppExp (TypeIdentExp (getIdentStr l) (getFuncType a)) (TypeIdentExp "a" (ps !! 0)) (getLeft (getFuncType a)))
	                 e2 = (TypeAppExp e1 (TypeIdentExp "c" (ps !! 1)) (getTExpType expr))
	             return e2

	getLeft (TypeFunc t t2) = t2

compileLetFuncApp i bind bexps expr = do
  let func = getExpFunc expr
      (bs, patts, fexpr) = fromJust $ lookup func bexps
      args = getExpFuncArgs expr
  cArgs <- mapM (compileExpr i bind bexps) args
  let abinds = map (\(_,b,_) -> b) cArgs
      asrc = concat $ map (\(s,_,_) -> s) cArgs 
      zap = zip abinds patts
  nbinds <- concat <$> mapM (\(a,p) -> bindParameter i p a) zap
  let fbinds = nbinds ++ bs
  (csrc, cbind, _) <- compileExpr i fbinds bexps fexpr
  return (asrc ++ csrc, cbind, [])

compileFuncApp :: ([TypeDef], [TypedFunc])
	       -> [(Ident, [Char])]
	       -> [(Ident, ([(Ident, [Char])], [TypePattern], TypeExp))]
	       -> TypeExp
	       -> StateT CState CompilerM ([String], [Char], [()])
compileFuncApp i bind bexps expr = do
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  if isTBaseFunc funcD
    then compileBaseFuncApp i bind bexps expr
    else compileRegFuncApp i bind bexps expr

compileRegFuncApp i bind bexps expr = do
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  let args = getExpFuncArgs expr
      patts = getFuncPattern funcD
  cArgs <- mapM (compileExpr i bind bexps) args
  let abinds = map (\(_,b,_) -> b) cArgs
      asrc = concat $ map (\(s,_,_) -> s) cArgs 
      zap = zip abinds patts
  nbinds <- concat <$> mapM (\(a,p) -> bindParameter i p a) zap
  let fbinds = nbinds ++ bind
  (csrc, cbind, _) <- compileExpr i fbinds bexps (getFuncExpr funcD)
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
	  | isTDataDecl funcT = do let params = getPattVars p
				       vars = map (\x -> var ++ ".m" ++ (show x)) [1..]
				       zpv = zip params vars
				   concat <$> mapM (\(q, v) -> bindParameter i q v) zpv
	  | otherwise = fail "bind not impl"

getFuncTypeParams (TypeFunc t d) = t:(getFuncTypeParams d)
getFuncTypeParams (TypeParen p) = getFuncTypeParams p
getFuncTypeParams _ = []

compileBaseFuncApp i bind bexps expr = do
  let args = getExpFuncArgs expr
  cArgs <- mapM (compileExpr i bind bexps) args
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
      

compileTypeCon i bind bexps expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  compileTypeCon' aD
  where compileTypeCon' aD
	  | isTSemantic aD = compileTSemantic i bind bexps expr
	  | isTBaseDef aD = compileTBaseDef i bind bexps expr
	  | isTDataDecl aD = compileTDataDecl i bind bexps expr
	  | otherwise = fail "not implemented compileTypeCon"

compileTSemantic i bind bexps expr = do
  args <- return $ getExpFuncArgs expr
  compileExpr i bind bexps (head args)

compileTBaseDef i bind bexps expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  func <- return $ getExpFunc expr
  funcD <- lookupFunc func i
  let args = getExpFuncArgs expr
  cArgs <- mapM (compileExpr i bind bexps) args
  let aBinds = map (\(_,b,_) -> b) cArgs
      aSrc = concat $ map (\(s,_,_) -> s) cArgs
      bStr = doTemplateReplace aBinds (getTBaseConstStr aD)
  return (aSrc, bStr, [])

compileTDataDecl i bind bexps expr = do
  a <- return $ getTypeCon $ returnType $ getExpFuncType expr
  aD <- lookupType a i
  if isTDataAlias aD
    then compileExpr i bind bexps (head $ getExpFuncArgs expr)
    else do let args = getExpFuncArgs expr
	    cArgs <- mapM (compileExpr i bind bexps) args
	    v <- freshVar
	    let dsrc = [(getIdentStr (getTypeCon (getTExpType expr))) ++ " " ++ v ++ ";"]
		aBinds = map (\(_,b,_) -> b) cArgs
		aSrc = concat $ map (\(s,_,_) -> s) cArgs
		names = map (\x -> v ++ ".m" ++ (show x)) [1..]
		zbn = zip aBinds names
		bSrc = map (\(b,n) -> n ++ " = " ++ b ++ ";") zbn
	    return (dsrc ++ aSrc ++ bSrc, v, [])

generateReturnType i t = do
  con <- return $ getTypeCon t
  conT <- lookupType con i
  generateRet' conT
  where generateRet' conT
	  | isTSemantic conT = generateSemanticReturn i t
	  | isTBaseDef conT = generateBaseDefReturn i t
	  | isTDataDecl conT = generateDataDeclReturn i t

generateDataDeclReturn i t = do
  con <- return $ getTypeCon t
  conT <- lookupType con i
  if isTDataAlias conT
    then generateReturnType i (getTDataAlias conT)
    else return (getIdentStr con, "")

generateBaseDefReturn i t = do
  ty <- return $ getTypeCon t
  tyDecl <- lookupType ty i
  return (getTBaseDefString tyDecl, "")

generateSemanticReturn i t = do
  ty <- return $ getTypeCon t
  tyDecl <- lookupType ty i
  a <- return $ getParamT t
  varTD <- lookupType (getTypeCon a) i
  if (isTBaseDef varTD)
    then return (getTBaseDefString varTD,": " ++ (getTSemanticString tyDecl))
    else fail "invalid semantic parameter"
  where getParamT (TypeApp _ t) = t
	getParamT (TypeParen t) = getParamT t

generateFuncParam i p = do
  v <- freshVar
  generateFuncParam' i v p

generateFuncParam' i v p = do
  con <- return $ getTypeCon $ getTPattType p
  conT <- lookupType con i
  generateParam' conT v
  where generateParam' conT v
	  | isTSemantic conT = generateSemanticParam i v p
	  | isTDataDecl conT = generateDataDeclParam i v p
	  | isTBaseDef conT = generateBaseDefParam i v p

generateBaseDefParam i v p = do
  con <- return $ getTypeCon $ getTPattType p
  conT <- lookupType con i
  return ([(getPattIdent p, v)], (getTBaseDefString conT) ++ " " ++ v)

generateDataDeclParam i v p = do
  con <- return $ getTypeCon $ getTPattType p
  conT <- lookupType con i
  if (isTDataAlias conT)
    then generateFuncParam' i v (makeNewPatt p (getTDataAlias conT))
    else do str <- return $ (getIdentStr con) ++ " " ++ v
	    binds <- bindParameter i p v
	    return (binds, str)
  where makeNewPatt (TypeIdentPattern i _) t = TypeIdentPattern i t
	makeNewPatt (TypeAppPattern _ b _) t = b
	makeNewPatt (TypeParenPattern p _) t = makeNewPatt p t

generateSemanticParam i v p = do
  ty <- return $ getTypeCon $ getTPattType p
  tyDecl <- lookupType ty i
  tvars <- return $ getTypeParams $ getTPattType p
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
	    (_, cStr, _) <- compileExpr i [] [] expr
	    str <- return $ bStr ++ " " ++ ide ++ " = " ++ cStr ++ ";"
	    return ((IdentVar ide, ide),[str])
    else do str <- return $ bStr ++ " " ++ ide ++ ";"
	    return ((IdentVar ide, ide),[str])

getTParamDecls fs = filter isTParamDecl fs
getTDataDecls ds = filter isTDataDecl ds

isTParamDecl (TypedParamDecl _ _) = True
isTParamDecl _ = False

{----------------------
 Compile structs
----------------------}

compileStructs i = do
  let ds = getTDataDecls $ fst i
      ss = filter (not . isTDataAlias) ds
  concat <$> mapM (compileStruct i) ss


--TypeDataDef Ident Kind TypedFunc (Maybe Type)
compileStruct i (TypeDataDef ident _ (TypedTypeConst _ ty) _) = do
  let tyCons = getFuncTypeParams ty
      vars = map (\x -> "m" ++ (show x)) (take (length tyCons) [1..])
      patts = map (\x -> TypeIdentPattern "a" x) tyCons
      vpzip = zip vars patts
  a <- mapM (\(v,p) -> generateFuncParam' i v p) vpzip
  let ls = map (\x -> (snd x) ++ ";") a
  return (["struct " ++ (getIdentStr ident) ++ "{"] ++ ls ++ ["};"])




{----------------------
 Entry point
----------------------}
  
compile' i = do
  (pbinds, psrc) <- compileParameters i
  ss <- compileStructs i
  let techniques = filter isTTechniqueDecl (fst i)
  ts <- mapM compileTechnique techniques
  let tsrc = concat $ map fst ts 
      tfncs = concat $ map snd ts
  fs <- mapM (compileF pbinds) tfncs 
  let src = psrc ++ [""] 
	    ++ ss ++ [""] 
	    ++ fs ++ [""]
	    ++ tsrc
      output = intercalate "\n" src
  return output
  where compileF pbinds ident = do
	  vf <- lookupFunc ident i
	  compileFunc i vf pbinds

	compileTechnique (TypeTechniqueDecl ident ps) = do
	  let ts = ["technique " ++ (getIdentStr ident) ++ "{"]
	  cps <- mapM compilePass ps
	  let s = concat $ map fst cps
	      f = concat $ map snd cps
	      te = ["};"]
	  return (ts ++ s ++ te, f)

	compilePass (PassDecl ident fs) = do
	  let ts = ["pass " ++ (getIdentStr ident) ++ "{"]
	  cps <- mapM compileFragment fs
	  let s = concat $ map fst cps
	      f = map snd cps
	      te = ["}"]
	  return (ts ++ s ++ te, f)

	compileFragment (stage, model, ident) = do
	  return ([stage ++ " = compile " ++ model ++ " " ++ (getIdentStr ident) ++ "();"], ident) 

compile a = do 
  (a, _) <- runStateT (compile' a) (CState 0)
  return a