{
module Parser 
  ( parseTokens 
  ) where

import Types
}

%name parseTokens
%tokentype { Lexeme }
%monad { ParseE }
%error { parseError }

%token
      data         { Lexeme _ _ (TokBuiltin "data") }
      basetype     { Lexeme _ _ (TokBuiltin "basetype") }
      basefunc     { Lexeme _ _ (TokBuiltin "basefunc") }
      semantic     { Lexeme _ _ (TokBuiltin "semantic") }
      '='          { Lexeme _ _ TokEquals }
      '::'         { Lexeme _ _ TokTypeSpec }
      '->'         { Lexeme _ _ TokRArrow }
      '('          { Lexeme _ _ TokOpenParen }
      ')'          { Lexeme _ _ TokCloseParen }
      ','          { Lexeme _ _ TokComma }
      ';'          { Lexeme _ _ TokColon }
      string       { Lexeme _ _ (TokStringLit $$) }
      int          { Lexeme _ _ (TokIntLit $$) }
      real         { Lexeme _ _ (TokRealLit $$) }
      con          { Lexeme _ _ (TokConId $$) }
      ident        { Lexeme _ _ (TokIdentId $$) }
      eof          { Lexeme _ _ TokEOF }

%%

Start :: { EffectModule } 
  : ModuleDecls eof             { EffectModule $1 }

ModuleDecls :: { [TopLevelDecl] }
  : TopLevelDecl ModuleDecls        { $1:$2 }
  | TopLevelDecl                    { [$1] }

TopLevelDecl :: { TopLevelDecl }
  : DataDecl                        { $1 }
  | FuncBind                        { $1 }
  | FuncTypeDecl                    { $1 }
  | BaseTypeDecl                    { $1 }
  | BaseFuncDecl                    { $1 }
  | SemanticDecl                    { $1 }

BaseTypeDecl :: { TopLevelDecl }
  : basetype con string '=' DataCon string ';' { BaseTypeDecl (Ident $2) $3 $5 $6 }

BaseFuncDecl :: { TopLevelDecl }
  : basefunc ident '::' Type string ';' { BaseFuncDecl (Ident $2) $4 $5 }

SemanticDecl :: { TopLevelDecl }
  : semantic con '=' DataCon string ';'   { SemanticDecl (Ident $2) [] $4 $5 }
  | semantic con DataParams '=' DataCon string ';' { SemanticDecl (Ident $2) $3 $5 $6 }

DataDecl :: { TopLevelDecl }         
  : data con '=' DataCon  ';'   { DataDecl (Ident $2) [] $4 }
  | data con DataParams '=' DataCon ';'  { DataDecl (Ident $2) $3 $5 }

DataCon :: { Constructor }
  : con                    { Constructor (Ident $1) [] }
  | con ADataCon           { Constructor (Ident $1) $2 }

ADataCon :: { [Type] }
  : AType ADataCon            { $1:$2 }
  | AType                     { [$1] }

DataParams :: { [Ident] }
  : ident DataParams         { (Ident $1):$2 }
  | ident                    { [Ident $1] }

FuncBind :: { TopLevelDecl }
  : ident FuncParams '=' Expr ';'   { FuncBindDecl (Ident $1) $2 $4 }
  | ident '=' Expr ';'              { FuncBindDecl (Ident $1) [] $3 }

FuncParams :: { [ Pattern ] }
  : AFuncParam FuncParams         { $1:$2 }
  | AFuncParam                    { [$1] }

AFuncParam :: { Pattern }
  : ident                         { IdentPattern $1 }
  | con                           { ConPattern $1 }
  | '(' BFuncParam ')'            { ParenPattern $2 }

BFuncParam :: { Pattern }
  : AFuncParam BFuncParam         { AppPattern $1 $2 }
  | AFuncParam                    { $1 }

FuncTypeDecl :: { TopLevelDecl }
  : ident '::' Type ';'      { FuncTypeDecl (Ident $1) $3 }

Type :: { Type }
  : BType                    { $1 }
  | BType '->' Type          { TypeFunc $1 $3 }

BType :: { Type }
  : AType BType              { TypeApp $1 $2 }
  | AType                    { $1 }

AType :: { Type }
  : con                      { TypeCon $1 }
  | ident                    { TypeVar $1 }
  | '(' Type ')'             { TypeParen $2 }

Expr :: { Exp }
  : Expr AExpr               { AppExp $1 $2 } 
  | AExpr                    { $1 }

AExpr :: { Exp }
  : LiteralExpr              { $1 }
  | '(' Expr ')'             { ParenExp $2 }
  | '(' Expr TupleEnd        { TupleExp ($2:$3) }
  | ident                    { IdentExp $1 }

TupleEnd :: { [Exp] }
  : ',' Expr TupleEnd        { $2:$3 }
  | ',' Expr ')'             { [$2] }

LiteralExpr :: { Exp }
  : string                   { LiteralExp (LiteralString $1) }
  | int                      { LiteralExp (LiteralInt $1) }
  | real                     { LiteralExp (LiteralReal $1) }


{ 
parseError ((Lexeme l c _):_) = fail $ "Parse error at " ++ (show l) ++ ":" ++ (show c)
parseError _ = fail "durffff"
}