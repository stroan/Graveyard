module Compiler 
  ( compile
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Types

{-- 
Monad used for compiling
--}
data CompilerM a = CompErr String 
                 | CompSuccess a
                 deriving (Show, Eq)

instance Monad CompilerM where
  return = CompSuccess
  (CompErr s) >>= g = CompErr s
  (CompSuccess a) >>= g = g a
  fail a = CompErr a

instance Functor CompilerM where
  f `fmap` (CompErr s) = CompErr s
  f `fmap` (CompSuccess a) = CompSuccess (f a)

data Kind = KindVar
          | KindApp Kind Kind
          deriving (Show, Eq)

data TypedBinding = TBBData Kind TopLevelDecl
                  | TBBCon Ident Type Constructor
                  | TBBFunc Type TopLevelDecl
                  deriving (Show, Eq)


normaliseType (TypeParen t) = t
normaliseType (TypeApp t1 t2) = TypeApp (normaliseType t1) (normaliseType t2)
normaliseType t = t

validateAndType :: EffectModule -> CompilerM [(Ident,TypedBinding)]
validateAndType (EffectModule tlds) =
  do baseDataDecls <- return $ filter isBaseData tlds
     env1 <- concat <$> mapM annotateBaseDataDecl baseDataDecls
     baseFuncDecls <- return $ filter isBaseFunc tlds
     env2 <- ((++) env1) <$> mapM (annotateBaseFuncDecl env1) baseFuncDecls
     semanticDecls <- return $ filter isSemantic tlds
     env3 <- ((++) env2) <$> mapM (annotateSemanticDecl env2) semanticDecls
     return env3
  where isBaseData (BaseTypeDecl _ _ _ _) = True
        isBaseData _ = False

        isBaseFunc (BaseFuncDecl _ _ _) = True
        isBaseFunc _ = False

        isSemantic (SemanticDecl _ _ _ _) = True
        isSemantic _ = False

        isLexType (IdentCon "Real") = True
        isLexType (IdentCon "Int") = True
        isLexType _ = False

        envContains env k = isJust $ lookup k env

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

        annotateSemanticDecl env d@(SemanticDecl t tvars c@(Constructor con contypes) _) =
          do fail "Can't type semantics"


compile (Ok m) = "Code to be generated :( : " ++ show (validateAndType m)
compile (Failed e) = "Compilation failed due to: " ++ e