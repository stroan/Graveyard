module Compiler 
  ( compile
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Types


validateAndType :: EffectModule -> CompilerM ([(Ident,TypedBinding)],[(Ident,UntypedFunc)])
validateAndType (EffectModule tlds) =
  do baseDataDecls <- return $ filter isBaseData tlds
     env1 <- concat <$> mapM annotateBaseDataDecl baseDataDecls
     baseFuncDecls <- return $ filter isBaseFunc tlds
     env2 <- ((++) env1) <$> mapM (annotateBaseFuncDecl env1) baseFuncDecls
     semanticDecls <- return $ filter isSemantic tlds
     env3 <- (((++) env2) . concat) <$> mapM (annotateSemanticDecl env2) semanticDecls
     funcBinds <- return $ filter isFuncBind tlds
     funcTypes <- return $ map (\f@(FuncTypeDecl i t) -> (i, t)) $ filter isFuncType tlds
     funcs <- mapM (annotateFuncBinds funcTypes) funcBinds
     return (env3, funcs)
  where envContains env k = isJust $ lookup k env

        annotateBaseDataDecl d@(BaseTypeDecl t _ c@(Constructor con contypes) _) = 
          do kind <- return KindVar
             contype <- return $ foldr (\a b -> TypeApp a b) (TypeCon t) contypes
             contype' <- return $ normaliseType contype
             if validBaseDataType contype'
               then return [(t, TBBData kind d), (con, TBBCon t contype' c)]
               else fail $ show contype'

        validBaseDataType (TypeApp (TypeCon con) t2) = isLexType con && validBaseDataType t2
        validBaseDataType (TypeCon con) = True

        annotateBaseFuncDecl env d@(BaseFuncDecl ident ty _) =
          if validBaseFuncType env ty 
            then return (ident, TBBFunc ty  d)
            else fail $ show d
        
        validBaseFuncType env (TypeFunc (TypeCon con) t2) = envContains env con && validBaseFuncType' env t2
        validBaseFuncType _ _ = False
        validBaseFuncType' env (TypeFunc (TypeCon con) t2) = envContains env con && validBaseFuncType' env t2
        validBaseFuncType' env (TypeCon con) = envContains env con

        annotateSemanticDecl env d@(SemanticDecl t [tvar] c@(Constructor con [ct]) _) =
          do kind <- return $ KindApp KindVar KindVar
	     contype <- return $ (TypeFunc ct (TypeApp (TypeCon t) (TypeVar tvar)))
             if (TypeVar tvar) == ct
		then return [(t, TBBData kind d), (con, TBBCon t contype c)]
		else fail $ show contype	  
	annotateSemanticDecl _ _ = fail "Invalid semantic decl"

	annotateFuncBinds funcTypes f@(FuncBindDecl i _ _) =
	  let t = lookup i funcTypes in
	  return (i, UTFunc t f)
	annotateFuncBinds _ _ = fail "Invalid function decl"


compile (Ok m) = "Code to be generated :( : " ++ show (validateAndType m)
compile (Failed e) = "Compilation failed due to: " ++ e