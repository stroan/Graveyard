module Parser.AST
  ( EffectModule (..), TopLevelDecl (..)
  , PassDecl (..), Ident (..), Exp (..)
  , Pattern (..), Literal (..), Constructor (..)
  , isDataDecl, isBaseData, isBaseFunc, isSemantic
  , isFuncBind, isFuncType, isParamDecl, isTechniqueDecl
  , getLiteralStr
  ) where 

import Types

data EffectModule = EffectModule [TopLevelDecl]
                    deriving (Show, Eq)

data TopLevelDecl = DataDecl Ident [Ident] Constructor 
                  | FuncBindDecl Ident [Pattern] Exp
                  | FuncTypeDecl Ident Type
                  | BaseTypeDecl Ident String (Maybe (Constructor, String)) Bool
                  | BaseFuncDecl Ident Type String
                  | SemanticDecl Ident [Ident] Constructor String
		  | ParamDecl Ident Type
		  | TechniqueDecl Ident [PassDecl]
                    deriving (Show, Eq)

data PassDecl = PassDecl Ident [(String, String, Ident)]
		deriving (Show, Eq)

data Exp = LiteralExp Literal
         | IdentExp String
         | ConsExp String
         | AppExp Exp Exp
         | ParenExp Exp
         | TupleExp [Exp]
	 | LetExp Ident [Pattern] Exp Exp
	 | IfExp Exp Exp Exp
	 | LoopExp Ident Ident Exp
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


data Constructor = Constructor Ident [Type]
                 deriving (Show, Eq)


isDataDecl (DataDecl _ _ _) = True
isDataDecl _ = False

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

isParamDecl (ParamDecl _ _) = True
isParamDecl _ = False

isTechniqueDecl (TechniqueDecl _ _) = True
isTechniqueDecl _ = False

getLiteralStr (LiteralInt s) = s
getLiteralStr (LiteralString s) = s
getLiteralStr (LiteralReal s) = s