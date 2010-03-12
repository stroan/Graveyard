module TypeChecker
  (
  typeCheck
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.List
import Data.Maybe
import Types

data Environ = Environ [(Ident, Type)]
	     deriving (Show, Eq)

data TCState = TCState { tcstateID :: Integer }
type TCStateM = StateT TCState CompilerM


freshTypeVar :: TCStateM Type
freshTypeVar = StateT (\s -> return (TypeId (tcstateID s), TCState $ (tcstateID s)+1))


{--------------
Type Checker
--------------}

getDataDecls (EffectModule tlds) = filter isDataDecl tlds
getBaseDatas (EffectModule tlds) = filter isBaseData tlds
getBaseFuncs (EffectModule tlds) = filter isBaseFunc tlds
getSemantics (EffectModule tlds) = filter isSemantic tlds
getFuncBinds (EffectModule tlds) = filter isFuncBind tlds
getFuncTypes (EffectModule tlds) = filter isFuncType tlds
getParamDecls (EffectModule tlds) = filter isParamDecl tlds
getTechniqueDecls (EffectModule tlds) = filter isTechniqueDecl tlds

{---
 Environment functions
---}

isEnvConflict :: Environ -> Environ -> Bool
isEnvConflict (Environ a) (Environ b) = 
  let as = map fst a
      bs = map fst b 
  in as `intersect` bs /= []

mergeEnvs :: Environ -> Environ -> Environ
mergeEnvs (Environ a) (Environ b) = 
  let bis = map fst b
      as = filter (\(i,_) -> not (i `elem` bis)) a
  in Environ (as ++ b)

mergeEnvsErrorM :: Monad m => String -> Environ -> Environ -> m Environ
mergeEnvsErrorM s a b =
  if isEnvConflict a b 
     then fail s
     else return $ mergeEnvs a b

envContains i (Environ es) = isJust $ lookup i es
fromEnviron i (Environ es) = fromJust $ lookup i es

getFromEnvironM i e =
  if envContains i e 
    then return $ fromEnviron i e
    else fail $ "Cannot find identifier " ++ (show i)

{--
TypeDefs and FuncDefs helpers
--}

typeDefId (TypeBaseDef i _ _ _ _) = i

typeDefsContain i ts = any (\x -> typeDefId x == i) ts


{---
 Base Type Decleration checking
---}

checkBaseTypeDecls :: EffectModule -> TCStateM ([TypeDef], [TypedFunc], Environ)
checkBaseTypeDecls m = do
  let tlds = getBaseDatas m
  results <- mapM checkBaseType tlds
  env <- lift $ foldM (mergeEnvsErrorM "Multiple declarations of base type") 
	 (Environ []) (map (\(_,_,x) -> x) results)
  return (map (\(x,_,_) -> x) results
	 , map fromJust (filter isJust (map (\(_,x,_) -> x) results))
	 , env)

checkBaseType :: TopLevelDecl -> TCStateM (TypeDef, Maybe TypedFunc, Environ)
checkBaseType (BaseTypeDecl i ts (Just ((Constructor ci cts),cs)) side) = do
  kind <- return KindVar
  contype <- return $ foldr (\a b -> TypeFunc a b) (TypeCon i) cts
  if validBaseDataType contype
     then return ( TypeBaseDef i kind (Just $ BaseConst ci contype cs) ts side
		 , Just $ TypedTypeConst ci contype
		 , Environ [(ci, contype)] )
     else lift $ fail "Invalid base type"
  where validBaseDataType (TypeFunc (TypeCon con) t2) = isLexType con && validBaseDataType t2
        validBaseDataType (TypeCon con) = True

checkBaseType (BaseTypeDecl i ts Nothing s) = do
  kind <- return KindVar
  return (TypeBaseDef i kind Nothing ts s
	 , Nothing
	 , Environ [])

checkBaseType _ = lift $ fail "Invalid base type"

{---
 Base Function Declaration checking
---}

checkBaseFuncDecls :: EffectModule -> [TypeDef] -> TCStateM ([TypedFunc], Environ)
checkBaseFuncDecls m tys = do
  let tlds = getBaseFuncs m
  results <- mapM (checkBaseFunc tys) tlds
  env <- lift $ foldM (mergeEnvsErrorM "Multiple declarations of base func")
	 (Environ []) (map snd results)
  return (map fst results, env)

checkBaseFunc :: [TypeDef] -> TopLevelDecl -> TCStateM (TypedFunc, Environ)
checkBaseFunc tys decl@(BaseFuncDecl ident ty s) = 
  if validBaseFuncType' ty
    then return (TypedBaseFunc ident ty s, Environ [(ident, ty)])
    else fail $ "invalid base func" ++ (show decl)
  where validBaseFuncType (TypeFunc (TypeCon con) t2) = typeDefsContain con tys && validBaseFuncType' t2
        validBaseFuncType _ = False
        validBaseFuncType' (TypeFunc (TypeCon con) t2) = typeDefsContain con tys && validBaseFuncType' t2
        validBaseFuncType' (TypeCon con) = typeDefsContain con tys

{---
  Check parameters
---}
checkParamDecls :: EffectModule -> [TypeDef] -> TCStateM ([TypedFunc], Environ)
checkParamDecls m tys = do
  let tlds = getParamDecls m
  results <- mapM (checkParamDecl tys) tlds
  env <- lift $ foldM (mergeEnvsErrorM "Multiple declarations of parameter")
	 (Environ []) (map snd results)
  return (map fst results, env)

checkParamDecl :: [TypeDef] -> TopLevelDecl -> TCStateM (TypedFunc, Environ)
checkParamDecl tys (ParamDecl i t) = do
  if validParamType t
    then return (TypedParamDecl (IdentVar ("param_" ++ getIdentStr i)) t, Environ [(i, t)])
    else fail "invalid parameter"
  where validParamType (TypeParen p) = validParamType p
	validParamType (TypeCon c) = typeDefsContain c tys
	validParamType _ = False

{---
 Semantic decl checking
---}

checkSemanticDecls m tys = do
  let tlds = getSemantics m
  results <- mapM (checkSemantic tys) tlds
  env <- lift $ foldM (mergeEnvsErrorM "Multiple declarations of semantic type") 
	 (Environ []) (map (\(_,_,x) -> x) results)
  return (map (\(x,_,_) -> x) results, map (\(_,x,_) -> x) results, env)

checkSemantic tys (SemanticDecl t [tvar] c@(Constructor con [ct]) s) = do
  kind <- return $ KindApp KindVar KindVar
  contype <- return $ (TypeGen tvar (TypeFunc ct (TypeApp (TypeCon t) (TypeVar tvar))))
  if (TypeVar tvar) == ct
    then return ( TypeSemanticDef t kind (SemanticConst con contype) s
		, TypedTypeConst con contype
		, Environ [(con, contype)] )
    else fail $ show contype	
checkSemantic _ _ = fail "Invalid semantic decl."

{---
 Data decl checking
---}

checkDataDecls m tys = do
  let tlds = getDataDecls m
  results <- mapM (checkDataDecl tys) tlds
  env <- lift $ foldM (mergeEnvsErrorM "Multiple declarations of type") 
	 (Environ []) (map (\(_,_,x) -> x) results)
  return (map (\(x,_,_) -> x) results, map (\(_,x,_) -> x) results, env)

checkDataDecl tys (DataDecl ident [] (Constructor cident ts)) = do
  let kind = KindVar
      contype = foldr (\a b -> TypeFunc a b) (TypeCon ident) ts
      consD = TypedTypeConst cident contype
      env = Environ [(cident, contype)]
  if (length ts) == 1 
    then return ((TypeDataDef ident kind consD (Just $ head ts))
		,consD, env)
    else return ((TypeDataDef ident kind consD Nothing)
		,consD, env)
  where validDataDecl (TypeFunc a t2) = validPType a && validDataDecl' t2
        validDataDecl _ = False
        validDataDecl' (TypeFunc a t2) = validPType a && validDataDecl' t2
        validDataDecl' (TypeCon con) = con == ident

	validPType ty = 
	  let con = getTypeCon ty
	      args = getTypeParams ty
	      conD = head $ filter (\x -> getTypeName x == con) tys
	      argCount = argsFromKind (getTypeKind conD)
	      vArgs = map validPType args
	  in ((length args) == argCount) && (and vArgs)

	argsFromKind (KindApp _ k) = 1 + argsFromKind k
	argsFromKind _ = 0

	
checkTechniques m fs = do
  let ts = getTechniqueDecls m
  mapM (checkTechnique fs) ts

checkTechnique fs (TechniqueDecl ident ps) = do
  mapM checkPass ps
  return (TypeTechniqueDecl ident ps)
  where checkPass (PassDecl i gs) = mapM checkFragment gs
	checkFragment (_,_,f) = do
	  let fl = null $ filter (\x -> getFuncName x == f) fs
	  if fl then fail "invalid technique"
		else return ()

{--
| TypeDataDef Ident Kind TypedFunc (Maybe Type)
TypedTypeConst Ident Type
--}

{---
 Function typing.
---}

checkFuncs m env = do
  let funcs = getFuncBinds m
      fts = getFuncTypes m
  typedFuncs <- mapM makeTypedFunc funcs
  env' <- return $ Environ (map (\(TypedFuncBind i t _ _) -> (i, t)) typedFuncs)
  cs <- concat <$> mapM (genFuncConstraints (mergeEnvs env env')) typedFuncs
  tcs <- mapM (genFuncTypeConstraint env') fts
  s <- (lift . unify) (cs ++ tcs)
  finalFuncs <- return $ map (applySubsToTFunc s) typedFuncs
  return finalFuncs
  where makeTypedFunc (FuncBindDecl ident ps expr) = do
	  v <- freshTypeVar
	  tps <- mapM makeTypedPattern ps
	  te <- makeTypedExp expr
	  return (TypedFuncBind ident v tps te)
	
	makeTypedPattern (IdentPattern i) = do 
	  v <- freshTypeVar
	  return (TypeIdentPattern i v)
	makeTypedPattern (ConPattern c) = do
	  v <- freshTypeVar
	  return (TypeConPattern c v)
	makeTypedPattern (ParenPattern p) = do
	  v <- freshTypeVar
	  t <- makeTypedPattern p
	  return (TypeParenPattern t v)
	makeTypedPattern (AppPattern p p') = do
	  v <- freshTypeVar
	  t <- makeTypedPattern p
	  t' <- makeTypedPattern p'
	  return (TypeAppPattern t t' v)
	
	makeTypedExp (LiteralExp l) = do
	  v <- freshTypeVar
	  return (TypeLiteralExp l v)
	makeTypedExp (IdentExp i) = do
	  v <- freshTypeVar
	  return (TypeIdentExp i v)
	makeTypedExp (ConsExp c) = do
	  v <- freshTypeVar
	  return (TypeConsExp c v)
	makeTypedExp (AppExp e e') = do
	  v <- freshTypeVar
	  t <- makeTypedExp e
	  t' <- makeTypedExp e'
	  return (TypeAppExp t t' v)
	makeTypedExp (ParenExp p) = do
	  v <- freshTypeVar
	  t <- makeTypedExp p
	  return (TypeParenExp t v)
	makeTypedExp (LetExp i e e2) = do
	  v <- freshTypeVar
	  t <- makeTypedExp e
	  t2 <- makeTypedExp e2
	  return (TypeLetExp i t t2 v)
	makeTypedExp (IfExp c t f) = do
	  v <- freshTypeVar
	  c' <- makeTypedExp c
	  t' <- makeTypedExp t
	  f' <- makeTypedExp f
	  return (TypeIfExp c' t' f' v)
	makeTypedExp (LoopExp l w i) = do
	  v <- freshTypeVar
	  i' <- makeTypedExp i
	  return (TypeLoopExp l w i' v)
	makeTypedExp (TupleExp es) = do
	  v <- freshTypeVar
	  ts <- mapM makeTypedExp es
	  return (TypeTupleExp ts v)

applySubsToTFunc s (TypedFuncBind i t ps e) = 
  let t' = applySubstitution s t
      ps' = map (applySubsToTPatt s) ps
      e' = applySubsToTExpr s e
  in TypedFuncBind i t' ps' e'

applySubsToTExpr s (TypeLiteralExp l t) = TypeLiteralExp l (applySubstitution s t)
applySubsToTExpr s (TypeIdentExp i t) = TypeIdentExp i (applySubstitution s t)
applySubsToTExpr s (TypeConsExp c t) = TypeConsExp c (applySubstitution s t)
applySubsToTExpr s (TypeAppExp e1 e2 t) = TypeAppExp (applySubsToTExpr s e1) (applySubsToTExpr s e2) (applySubstitution s t)
applySubsToTExpr s (TypeParenExp p t) = TypeParenExp (applySubsToTExpr s p) (applySubstitution s t)
applySubsToTExpr s (TypeLetExp i t1 t2 t) = TypeLetExp i (applySubsToTExpr s t1) (applySubsToTExpr s t2) (applySubstitution s t)
applySubsToTExpr s (TypeIfExp c t f ty) = TypeIfExp (applySubsToTExpr s c) (applySubsToTExpr s t) (applySubsToTExpr s f) (applySubstitution s ty)
applySubsToTExpr s (TypeLoopExp l w i ty) = TypeLoopExp l w (applySubsToTExpr s i) (applySubstitution s ty)
applySubsToTExpr s (TypeTupleExp es t) = undefined

applySubsToTPatt s (TypeIdentPattern i t) = TypeIdentPattern i (applySubstitution s t)
applySubsToTPatt s (TypeConPattern c t) = TypeConPattern c (applySubstitution s t)
applySubsToTPatt s (TypeParenPattern p t) = TypeParenPattern (applySubsToTPatt s p) (applySubstitution s t)
applySubsToTPatt s (TypeAppPattern p1 p2 t) = TypeAppPattern (applySubsToTPatt s p1) (applySubsToTPatt s p2) (applySubstitution s t)

	
genFuncConstraints env (TypedFuncBind i t ps expr) = do
  let n = foldr (\p r -> TypeFunc (getTPattType p) r) (getTExpType expr) ps
      env' =  mergeEnvs env (Environ $ concat (map getBindings ps))
  pcs <- concat <$> mapM (genPattConstraints env) ps
  cs <- genExprConstraints env' expr
  return $ (t, n):(cs ++ pcs)
  where getBindings (TypeIdentPattern s t) = [(IdentVar s, t)]
	getBindings (TypeParenPattern p t) = getBindings p
	getBindings (TypeAppPattern p p' t) = (getBindings p) ++ (getBindings p')
	getBindings _ = []

genFuncTypeConstraint env (FuncTypeDecl i t) = do
  a <- getFromEnvironM i env
  return (a, t)

genPattConstraints env (TypeConPattern s t) = do
  t' <- getFromEnvironM (IdentCon s) env
  if (isGenType t') 
    then do it <- instanceGeneric t'
	    return [(t, it)]
    else return [(t, t')]

genPattConstraints env (TypeParenPattern p t) = do
  a <- genPattConstraints env p
  return ([(t, getTPattType p)] ++ a)

genPattConstraints env p@(TypeAppPattern p1 p2 t) = do
  a <- genPattConstraints env p1
  a' <- genPattConstraints env p2
  return ([(getTPattType p1, TypeFunc (getTPattType p2) t)] ++ a ++ a')

genPattConstraints _ _ = return []

genExprConstraints env (TypeIdentExp s t) = do
  t' <- getFromEnvironM (IdentVar s) env
  return [(t, t')]

genExprConstraints env (TypeConsExp s t) = do
  t' <- getFromEnvironM (IdentCon s) env
  if (isGenType t') 
    then do it <- instanceGeneric t'
	    return [(t, it)]
    else return [(t, t')]

genExprConstraints env (TypeAppExp t1 t2 t) = do
  a <- genExprConstraints env t1
  a' <- genExprConstraints env t2
  return ([(getTExpType t1, TypeFunc (getTExpType t2) t)] ++ a ++ a')

genExprConstraints env (TypeParenExp e t) = do
  a <- genExprConstraints env e
  return ([(t, getTExpType e)] ++ a)

genExprConstraints env (TypeLetExp i t1 t2 t) = do
  a <- genExprConstraints env t1
  let ae = Environ [(i, getTExpType t1)]
  a2 <- genExprConstraints (mergeEnvs env ae) t2
  return ([(t, getTExpType t2)] ++ a ++ a2)

genExprConstraints env (TypeIfExp c t f ty) = do
  t' <- genExprConstraints env t
  f' <- genExprConstraints env f
  c' <- genExprConstraints env c
  return ([(TypeCon (IdentCon "Bool"), getTExpType c)
	  ,(getTExpType t, getTExpType f)
	  ,(ty, getTExpType t)] ++ t' ++ f' ++ c') 

genExprConstraints env (TypeLoopExp l w s ty) = do
  s' <- genExprConstraints env s
  lt <- getFromEnvironM l env
  wt <- getFromEnvironM w env
  return ([(ty, getTExpType s)
	  ,(lt, (TypeFunc (getTExpType s) (TypeFunc (TypeCon (IdentCon "Int")) (getTExpType s))))
	  ,(wt, (TypeFunc (getTExpType s) (TypeFunc (TypeCon (IdentCon "Int")) (TypeCon (IdentCon "Bool")))))]
	  ++ s')

genExprConstraints env (TypeLiteralExp (LiteralInt _) t) = return [(t, TypeCon (IdentCon "Integer"))]
genExprConstraints env (TypeLiteralExp (LiteralReal _) t) = return [(t, TypeCon (IdentCon "Real"))]
genExprConstraints env (TypeLiteralExp (LiteralString _) t) = return [(t, TypeCon (IdentCon "String"))]

instanceGeneric (TypeGen v t) = do
  v' <- freshTypeVar
  return $ replaceType v' t
  where replaceType v' a@(TypeVar w) = if w == v
					then v'
					else a
	replaceType v' (TypeFunc t t') = TypeFunc (replaceType v' t) (replaceType v' t')
	replaceType v' (TypeParen t) = TypeParen (replaceType v' t)
	replaceType v' (TypeApp t t') = TypeApp (replaceType v' t) (replaceType v' t')
	replaceType _ t = t

{-- Entry point --}

--typeCheck' :: EffectModule -> TCStateM String
typeCheck' a = do
  (btypes, btcons, env1) <- checkBaseTypeDecls a
  (bfuncs, env2) <- checkBaseFuncDecls a btypes
  (pfuncs, penv) <- checkParamDecls a btypes
  (stypes, scons, env3) <- checkSemanticDecls a btypes
  let envs = [env1, env2, penv, env3]
      env4 = foldl mergeEnvs (Environ []) envs
  (dtypes, dfuncs, denv) <- checkDataDecls a (btypes ++ stypes) 
  funcs <- checkFuncs a (mergeEnvs env4 denv)
  techs <- checkTechniques a funcs
  let rfuncs = concat [btcons, bfuncs, pfuncs, scons, dfuncs, funcs] 
  let rtypes = concat [btypes, stypes, dtypes, techs]
  return (rtypes, rfuncs)

typeCheck a = do 
  (a, _) <- runStateT (typeCheck' a) (TCState 0)
  return a

{--------------
 Unifier
--------------}

idSubstitution = []

makeSubstitution i t = [(i, t)]
composeSubstitution a b =
  let b' = filter (\(i,_) -> isNothing $ lookup i a) b
      b'' = map (\(i,t) -> (i, applySubstitution a t)) b'
  in (a ++ b'')
      
applySubstitution a t@(TypeId _) =
  let r = lookup t a 
  in if isJust r then fromJust r
		 else t
applySubstitution a (TypeApp t t') = TypeApp (applySubstitution a t) (applySubstitution a t')
applySubstitution a (TypeFunc t t') = TypeFunc (applySubstitution a t) (applySubstitution a t')
applySubstitution a (TypeParen t) = TypeParen (applySubstitution a t)
applySubstitution a b = b

applySubstitutions a bs = map (\(t,t') -> (applySubstitution a t, applySubstitution a t')) bs

occours tid (TypeId tid') = tid == tid'
occours tid (TypeFunc t t') = (occours tid t) || (occours tid t')
occours tid (TypeApp t t') = (occours tid t) || (occours tid t')
occours tid _ = False

unifyLoop [] s = return s
unifyLoop (tc@(t,t'):tcs) s 
  | t == t' = unifyLoop tcs s
  | otherwise = case tc of
		  ((TypeFunc t1 t2), (TypeFunc t1' t2')) -> unifyLoop ((t1,t1'):(t2,t2'):tcs) s
		  ((TypeApp t1 t2), (TypeApp t1' t2')) -> unifyLoop ((t1,t1'):(t2,t2'):tcs) s
		  (tid@(TypeId tidv), t2) -> if occours tidv t'
					   then fail "unification failure: infinite type"
					   else let a = makeSubstitution tid t2 in
						unifyLoop (applySubstitutions a tcs) (composeSubstitution a s)
		  (t1, (TypeId tid)) -> unifyLoop (((TypeId tid), t1):tcs) s
		  _ -> fail $ "unification failure: bad case - " ++ (show tc)

unify :: [(Type, Type)] -> CompilerM ([(Type,Type)])
unify tcs = do
  let ntcs = map (\(t1,t2) -> (normaliseType t1, normaliseType t2)) tcs
  unifyLoop ntcs idSubstitution

