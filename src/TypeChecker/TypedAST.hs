module TypeChecker.TypedAST
  ( TypeDef(..), BaseConst(..), SemanticConst(..)
  , TypedFunc(..), TypeExp(..), TypePattern(..)
  , getTExpType, getTPattType, getFuncName
  , getTypeName, getTypeKind
  , PassDecl(..), getLiteralStr
  ) where

import Parser.AST
import Types

data TypeDef = TypeBaseDef Ident Kind (Maybe BaseConst) String Bool
	     | TypeSemanticDef Ident Kind SemanticConst String
	     | TypeDataDef Ident Kind TypedFunc (Maybe Type)
	     | TypeTechniqueDecl Ident [PassDecl]
	     deriving (Show, Eq)

data BaseConst = BaseConst Ident Type String
	       deriving (Show, Eq)

data SemanticConst = SemanticConst Ident Type
		   deriving (Show, Eq)

data TypedFunc = TypedTypeConst Ident Type
	       | TypedBaseFunc Ident Type String
	       | TypedFuncBind Ident Type [TypePattern] TypeExp
	       | TypedParamDecl Ident Type
	       deriving (Show, Eq)

data TypeExp = TypeLiteralExp Literal Type
         | TypeIdentExp String Type
         | TypeConsExp String Type
         | TypeAppExp TypeExp TypeExp Type
         | TypeParenExp TypeExp Type
	 | TypeLetExp Ident [TypePattern] TypeExp TypeExp Type
         | TypeTupleExp [TypeExp] Type
	 | TypeIfExp TypeExp TypeExp TypeExp Type
	 | TypeLoopExp Ident Ident TypeExp Type
         | TypeLambdaExp TypePattern TypeExp
           deriving (Show, Eq)

data TypePattern = TypeIdentPattern String Type
             | TypeConPattern String Type
             | TypeParenPattern TypePattern Type
             | TypeAppPattern TypePattern TypePattern Type
             deriving (Show, Eq)


getTypeName (TypeBaseDef i _ _ _ _) = i
getTypeName (TypeSemanticDef i _ _ _) = i
getTypeName (TypeDataDef i _ _ _) = i

getTypeKind (TypeBaseDef _ k _ _ _) = k
getTypeKind (TypeSemanticDef _ k _ _) = k
getTypeKind (TypeDataDef _ k _ _) = k

isTParamDecl (TypedParamDecl _ _) = True
isTParamDecl _ = False

getTExpType (TypeLiteralExp _ t) = t
getTExpType (TypeIdentExp _ t) = t
getTExpType (TypeConsExp _ t) = t
getTExpType (TypeAppExp _ _ t) = t
getTExpType (TypeParenExp _ t) = t
getTExpType (TypeTupleExp _ t) = t
getTExpType (TypeLetExp _ _ _ _ t) = t
getTExpType (TypeIfExp _ _ _ t) = t
getTExpType (TypeLoopExp _ _ _ t) = t

getTPattType (TypeIdentPattern _ t) = t
getTPattType (TypeConPattern _ t) = t
getTPattType (TypeParenPattern _ t) = t
getTPattType (TypeAppPattern _ _ t) = t

getFuncName (TypedTypeConst i _) = i
getFuncName (TypedBaseFunc i _ _) = i
getFuncName (TypedFuncBind i _ _ _) = i
getFuncName (TypedParamDecl i _) = i