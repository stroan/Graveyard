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


data TypeDef = TypeBaseDef Ident Kind BaseConst String
	     | TypeSemanticDef Ident Kind SemanticConst String
	     deriving (Show, Eq)

data BaseConst = BaseConst Ident Type String
	       deriving (Show, Eq)

data SemanticConst = SemanticConst Ident Type
		   deriving (Show, Eq)

data TypedFunc = TypedTypeConst Ident Type
	       | TypedBaseFunc Ident Type String
	       | TypedFuncBind Ident Type [TypePattern] TypeExp
	       deriving (Show, Eq)

data TypeExp = TypeLiteralExp Literal Type
         | TypeIdentExp String Type
         | TypeConsExp String Type
         | TypeAppExp TypeExp TypeExp Type
         | TypeParenExp TypeExp Type
         | TypeTupleExp [TypeExp] Type
           deriving (Show, Eq)

       
data TypePattern = TypeIdentPattern String Type
             | TypeConPattern String Type
             | TypeParenPattern TypePattern Type
             | TypeAppPattern TypePattern TypePattern Type
             deriving (Show, Eq)

data Environ = Environ [(Ident, Type)]
	     deriving (Show, Eq)

data TCState = TCState { tcstateID :: Integer }
type TCStateM = StateT TCState CompilerM


freshTypeVar :: TCStateM Type
freshTypeVar = StateT (\s -> return (TypeId (tcstateID s), TCState $ (tcstateID s)+1))

getTExpType (TypeLiteralExp _ t) = t
getTExpType (TypeIdentExp _ t) = t
getTExpType (TypeConsExp _ t) = t
getTExpType (TypeAppExp _ _ t) = t
getTExpType (TypeParenExp _ t) = t
getTExpType (TypeTupleExp _ t) = t

getTPattType (TypeIdentPattern _ t) = t
getTPattType (TypeConPattern _ t) = t
getTPattType (TypeParenPattern _ t) = t
getTPattType (TypeAppPattern _ _ t) = t

isGenType (TypeGen _ _) = True
isGenType _ = False

{--------------
Type Checker
--------------}

getBaseDatas (EffectModule tlds) = filter isBaseData tlds
getBaseFuncs (EffectModule tlds) = filter isBaseFunc tlds
getSemantics (EffectModule tlds) = filter isSemantic tlds
getFuncBinds (EffectModule tlds) = filter isFuncBind tlds
getFuncTypes (EffectModule tlds) = filter isFuncType tlds

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

typeDefId (TypeBaseDef i _ _ _) = i

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
  return (map (\(x,_,_) -> x) results, map (\(_,x,_) -> x) results, env)

checkBaseType :: TopLevelDecl -> TCStateM (TypeDef, TypedFunc, Environ)
checkBaseType (BaseTypeDecl i ts (Constructor ci cts) cs) = do
  kind <- return KindVar
  contype <- return $ foldr (\a b -> TypeApp a b) (TypeCon i) cts
  if validBaseDataType contype
     then return ( TypeBaseDef i kind (BaseConst ci contype cs) ts
		 , TypedTypeConst ci contype
		 , Environ [(ci, contype)] )
     else lift $ fail "Invalid base type"
  where validBaseDataType (TypeApp (TypeCon con) t2) = isLexType con && validBaseDataType t2
        validBaseDataType (TypeCon con) = True
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
checkBaseFunc tys (BaseFuncDecl ident ty s) = 
  if validBaseFuncType ty
    then return (TypedBaseFunc ident ty s, Environ [(ident, ty)])
    else fail "invalid base func"
  where validBaseFuncType (TypeFunc (TypeCon con) t2) = typeDefsContain con tys && validBaseFuncType' t2
        validBaseFuncType _ = False
        validBaseFuncType' (TypeFunc (TypeCon con) t2) = typeDefsContain con tys && validBaseFuncType' t2
        validBaseFuncType' (TypeCon con) = typeDefsContain con tys

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
  return (finalFuncs)
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
applySubsToTExpr s (TypeTupleExp es t) = undefined

applySubsToTPatt s (TypeIdentPattern i t) = TypeIdentPattern i (applySubstitution s t)
applySubsToTPatt s (TypeConPattern c t) = TypeConPattern c (applySubstitution s t)
applySubsToTPatt s (TypeParenPattern p t) = TypeParenPattern (applySubsToTPatt s p) (applySubstitution s t)
applySubsToTPatt s (TypeAppPattern p1 p2 t) = TypeAppPattern (applySubsToTPatt s p1) (applySubsToTPatt s p2) (applySubstitution s t)

{--

data TypePattern = TypeIdentPattern String Type
             | TypeConPattern String Type
             | TypeParenPattern TypePattern Type
             | TypeAppPattern TypePattern TypePattern Type
             deriving (Show, Eq)
--}
	
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

genPattConstraints env (TypeAppPattern p1 p2 t) = do
  a <- genPattConstraints env p1
  a' <- genPattConstraints env p2
  return ([(getTPattType p1, TypeApp (getTPattType p2) t)] ++ a ++ a')

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

genExprConstraints env (TypeLiteralExp (LiteralInt _) t) = return [(t, TypeCon (IdentCon "Integer"))]
genExprConstraints env (TypeLiteralExp (LiteralReal _) t) = return [(t, TypeCon (IdentCon "Real"))]

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

{--

data Type = TypeCon Ident
          | TypeVar Ident
          | TypeFunc Type Type
          | TypeParen Type
          | TypeApp Type Type
	  | TypeId Integer
	  | TypeGen Ident Type
          deriving (Show, Eq)

--}

{-- Entry point --}

typeCheck' :: EffectModule -> TCStateM String
typeCheck' a = do
  (btypes, btcons, env1) <- checkBaseTypeDecls a
  (bfuncs, env2) <- checkBaseFuncDecls a btypes
  (stypes, scons, env3) <- checkSemanticDecls a btypes
  let env4 = mergeEnvs (mergeEnvs env1 env2) env3
  env5 <- checkFuncs a env4
  return $ intercalate "-----------" (map show env5)

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
applySubstitution a (TypeFunc t t') = TypeApp (applySubstitution a t) (applySubstitution a t')
applySubstitution a (TypeParen t) = TypeParen (applySubstitution a t)
applySubstitution a b = b

applySubstitutions a bs = map (\(t,t') -> (applySubstitution a t, applySubstitution a t')) bs

{--
idSubstitution = snd

makeSubstitution i t = (\(x,y) -> if i == x then t else y)
composeSubstitution a b = (\x@(y,z) -> let n = a x in if n == z then b x else n)

applySubstitution a t@(TypeId tid) = a (tid, t)
applySubstitution a (TypeApp t t') = TypeApp (applySubstitution a t) (applySubstitution a t')
applySubstitution a (TypeFunc t t') = TypeApp (applySubstitution a t) (applySubstitution a t')
applySubstitution a (TypeParen t) = TypeParen (applySubstitution a t)
applySubstitution a b = b

applySubstitutions a bs = map (\(t,t') -> (applySubstitution a t, applySubstitution a t')) bs
--}
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
		  _ -> fail "unification failure: bad case"

unify :: [(Type, Type)] -> CompilerM ([(Type,Type)])
unify tcs = unifyLoop tcs idSubstitution

