module Types
  ( Type (..), Kind (..), Ident (..)
  , normaliseType, getIdentStr, getTypeCon
  , getTypeParams, isGenType, isLexType
  , prettyTypeString
  ) where


data Ident = IdentVar String
           | IdentCon String
            deriving (Show, Eq)

getIdentStr (IdentVar i) = i
getIdentStr (IdentCon c) = c

data Type = TypeCon Ident
          | TypeVar Ident
          | TypeFunc Type Type
          | TypeParen Type
          | TypeApp Type Type
	  | TypeId Integer
	  | TypeGen Ident Type
          deriving (Show, Eq)

data Kind = KindVar
          | KindApp Kind Kind
          deriving (Show, Eq)

normaliseType (TypeParen t) = normaliseType t
normaliseType (TypeApp t1 t2) = TypeApp (normaliseType t1) (normaliseType t2)
normaliseType (TypeFunc t1 t2) = TypeFunc (normaliseType t1) (normaliseType t2)
normaliseType t = t

getTypeParams (TypeApp t d) = (getTypeParams t) ++ [d]
getTypeParams (TypeParen p) = getTypeParams p
getTypeParams _ = []

getTypeCon (TypeApp t _) = getTypeCon t
getTypeCon (TypeCon c) = c
getTypeCon (TypeParen t) = getTypeCon t

isGenType (TypeGen _ _) = True
isGenType _ = False

isLexType (IdentCon "Real") = True
isLexType (IdentCon "Integer") = True
isLexType (IdentCon "String") = True
isLexType _ = False

prettyTypeString (TypeCon con) = getIdentStr con
prettyTypeString (TypeVar var) = getIdentStr var
prettyTypeString (TypeFunc t t') = (prettyTypeString t) ++ " -> (" ++ (prettyTypeString t') ++ ")"
prettyTypeString (TypeParen t) = "(" ++ (prettyTypeString t) ++ ")"
prettyTypeString (TypeApp t t') = (prettyTypeString t) ++ " " ++ (prettyTypeString t')
prettyTypeString (TypeId i) = "?" ++ (show i)
prettyTypeString (TypeGen ident t) = "forall " ++ (getIdentStr ident) ++ ". " ++ (prettyTypeString t) 