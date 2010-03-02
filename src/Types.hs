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
  , normaliseType
  , isBaseData, isBaseFunc, isSemantic, isFuncBind, isFuncType, isLexType
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
                  | BaseTypeDecl Ident String Constructor String
                  | BaseFuncDecl Ident Type String
                  | SemanticDecl Ident [Ident] Constructor String
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


isBaseData (BaseTypeDecl _ _ _ _) = True
isBaseData _ = False

isBaseFunc (BaseFuncDecl _ _ _) = True
isBaseFunc _ = False

isSemantic (SemanticDecl _ _ _ _) = True
isSemantic _ = False

isFuncBind (FuncBindDecl _ _ _) = True
isFuncBind _ = False

isFuncType (FuncTypeDecl _ _) = True
isFuncType _ = False

isLexType (IdentCon "Real") = True
isLexType (IdentCon "Int") = True
isLexType _ = False


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

instance Monad CompilerM where
  return = CompSuccess
  (CompErr s) >>= g = CompErr s
  (CompSuccess a) >>= g = g a
  fail a = CompErr a

instance Functor CompilerM where
  f `fmap` (CompErr s) = CompErr s
  f `fmap` (CompSuccess a) = CompSuccess (f a)

