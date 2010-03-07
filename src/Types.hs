module Types
  ( 
  {-- Lexer types --}
  Token (..)
  , Lexeme (..)
  {-- Parser types --}
  , ParseE (..)
  , EffectModule (..)
  , TopLevelDecl (..)
  , Ident (..)
  , Exp (..)
  , Pattern (..)
  , Literal (..)
  , Type (..)
  , Constructor (..)
  {-- Compiler types --}
  , CompilerM (..)
  , Kind (..)
  , TypedBinding (..)
  , UntypedFunc (..)
  , fromCompilerM
  , normaliseType
  , isBaseData, isBaseFunc, isSemantic, isFuncBind, isFuncType, isParamDecl, isLexType
  , TypeDef(..), BaseConst(..), SemanticConst(..), TypedFunc(..), TypeExp(..), TypePattern(..)
  , getTExpType, getTPattType, isGenType
  ) where

{-- -- -- -- -- -- -- -- -- -- -- -- --
Type defining the tokens outputted by the lexer.
-- -- -- -- -- -- -- -- -- -- -- -- -- --}
data Token = TokBuiltin String
           | TokEquals
           | TokTypeSpec
           | TokRArrow
           | TokOpenParen
           | TokCloseParen
           | TokComma
           | TokColon
           | TokStringLit String
           | TokIntLit String
           | TokRealLit String
           | TokConId String
           | TokIdentId String
           | TokError
           | TokEOF
           deriving (Eq,Show)

data Lexeme = Lexeme Int Int Token
              deriving (Show, Eq)

{-- -- -- -- -- -- -- -- -- -- -- -- --
Types defining the AST
-- -- -- -- -- -- -- -- -- -- -- -- -- --}
data EffectModule = EffectModule [TopLevelDecl]
                    deriving (Show, Eq)

data TopLevelDecl = DataDecl Ident [Ident] Constructor 
                  | FuncBindDecl Ident [Pattern] Exp
                  | FuncTypeDecl Ident Type
                  | BaseTypeDecl Ident String (Maybe (Constructor, String))
                  | BaseFuncDecl Ident Type String
                  | SemanticDecl Ident [Ident] Constructor String
		  | ParamDecl Ident Type
                    deriving (Show, Eq)

data Ident = IdentVar String
           | IdentCon String
            deriving (Show, Eq)

data Exp = LiteralExp Literal
         | IdentExp String
         | ConsExp String
         | AppExp Exp Exp
         | ParenExp Exp
         | TupleExp [Exp]
	 | LetExp Ident Exp Exp
           deriving (Show, Eq)

data Literal = LiteralInt String
             | LiteralReal String
             | LiteralString String
             deriving (Show, Eq)
           
data Pattern = IdentPattern String
             | ConPattern String
             | ParenPattern Pattern
             | AppPattern Pattern Pattern
             deriving (Show, Eq)

data Type = TypeCon Ident
          | TypeVar Ident
          | TypeFunc Type Type
          | TypeParen Type
          | TypeApp Type Type
	  | TypeId Integer
	  | TypeGen Ident Type
          deriving (Show, Eq)

data Constructor = Constructor Ident [Type]
                 deriving (Show, Eq)



data Kind = KindVar
          | KindApp Kind Kind
          deriving (Show, Eq)

data TypedBinding = TBBData Kind TopLevelDecl
                  | TBBCon Ident Type Constructor
                  | TBBFunc Type TopLevelDecl
                  deriving (Show, Eq) 

data UntypedFunc = UTFunc (Maybe Type) TopLevelDecl
		 deriving (Show, Eq)



normaliseType (TypeParen t) = t
normaliseType (TypeApp t1 t2) = TypeApp (normaliseType t1) (normaliseType t2)
normaliseType t = t


isBaseData (BaseTypeDecl _ _ _) = True
isBaseData _ = False

isBaseFunc (BaseFuncDecl _ _ _) = True
isBaseFunc _ = False

isSemantic (SemanticDecl _ _ _ _) = True
isSemantic _ = False

isFuncBind (FuncBindDecl _ _ _) = True
isFuncBind _ = False

isFuncType (FuncTypeDecl _ _) = True
isFuncType _ = False

isParamDecl (ParamDecl _ _) = True
isParamDecl _ = False

isLexType (IdentCon "Real") = True
isLexType (IdentCon "Int") = True
isLexType _ = False


{--
Typed data decls
TypeDef(..), BaseConst(..), SemanticConst(..), TypedFunc(..), TypeExp(..), TypePattern(..),
getTExpType, getTPattType
--}

data TypeDef = TypeBaseDef Ident Kind (Maybe BaseConst) String
	     | TypeSemanticDef Ident Kind SemanticConst String
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
	 | TypeLetExp Ident TypeExp TypeExp Type
         | TypeTupleExp [TypeExp] Type
           deriving (Show, Eq)

data TypePattern = TypeIdentPattern String Type
             | TypeConPattern String Type
             | TypeParenPattern TypePattern Type
             | TypeAppPattern TypePattern TypePattern Type
             deriving (Show, Eq)


isTParamDecl (TypedParamDecl _ _) = True
isTParamDecl _ = False


getTExpType (TypeLiteralExp _ t) = t
getTExpType (TypeIdentExp _ t) = t
getTExpType (TypeConsExp _ t) = t
getTExpType (TypeAppExp _ _ t) = t
getTExpType (TypeParenExp _ t) = t
getTExpType (TypeTupleExp _ t) = t
getTExpType (TypeLetExp _ _ _ t) = t

getTPattType (TypeIdentPattern _ t) = t
getTPattType (TypeConPattern _ t) = t
getTPattType (TypeParenPattern _ t) = t
getTPattType (TypeAppPattern _ _ t) = t

isGenType (TypeGen _ _) = True
isGenType _ = False

{--
Monad for use in parsing, and contains resulting value.
--}
data ParseE a = Ok a 
              | Failed String
              deriving (Eq,Show)
  
instance Monad ParseE where
  return a = Ok a
  m >>= k = 
    case m of
         Ok a -> k a
         Failed e -> Failed e
  fail a = Failed a


data CompilerM a = CompErr String 
                 | CompSuccess a 
                 deriving (Show, Eq)

fromCompilerM (CompSuccess a) = a

instance Monad CompilerM where
  return = CompSuccess
  (CompErr s) >>= g = CompErr s
  (CompSuccess a) >>= g = g a
  fail a = CompErr a

instance Functor CompilerM where
  f `fmap` (CompErr s) = CompErr s
  f `fmap` (CompSuccess a) = CompSuccess (f a)

