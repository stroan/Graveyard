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
      parameter	   { Lexeme _ _ (TokBuiltin "parameter") }
      let          { Lexeme _ _ (TokBuiltin "let") }
      in           { Lexeme _ _ (TokBuiltin "in") }
      if           { Lexeme _ _ (TokBuiltin "if") }
      then         { Lexeme _ _ (TokBuiltin "then") }
      else         { Lexeme _ _ (TokBuiltin "else") }
      loop         { Lexeme _ _ (TokBuiltin "loop") }
      while        { Lexeme _ _ (TokBuiltin "while") }
      with         { Lexeme _ _ (TokBuiltin "with") }
      technique    { Lexeme _ _ (TokBuiltin "technique") }
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
  | ParamDecl                       { $1 }
  | TechniqueDecl                   { $1 }

BaseTypeDecl :: { TopLevelDecl }
  : basetype con string BaseTypeCon ';' { BaseTypeDecl (IdentCon $2) $3 $4 }

BaseTypeCon :: { Maybe (Constructor, String) }
  : '=' DataCon string                  { Just ($2,$3) }
  |                                     { Nothing }

BaseFuncDecl :: { TopLevelDecl }
  : basefunc ident '::' Type string ';' { BaseFuncDecl (IdentVar $2) $4 $5 }

TechniqueDecl :: { TopLevelDecl }
  : technique ident '=' '(' PassDecl ';' PassDecls ')' ';' { TechniqueDecl (IdentVar $2) ($5:$7) }

PassDecl :: { PassDecl }
  : ident '=' '(' FragmentDecl ';' FragmentDecls ')'      { PassDecl (IdentVar $1) ($4:$6) }

PassDecls :: { [PassDecl] }
  : PassDecl ';' PassDecls                    { $1:$3 }
  |                                           { [] }

FragmentDecls :: { [(String, String, Ident)] }
  : FragmentDecl ';' FragmentDecls            { $1:$3 }
  |                                       { [] }

FragmentDecl :: { (String, String, Ident) }
  : string '=' ident '::' string          { ($1,$5,IdentVar $3) }

ParamDecl :: { TopLevelDecl }
  : parameter ident '::' Type ';'       { ParamDecl (IdentVar $2) $4 }

SemanticDecl :: { TopLevelDecl }
  : semantic con '=' DataCon string ';'   { SemanticDecl (IdentCon $2) [] $4 $5 }
  | semantic con DataParams '=' DataCon string ';' { SemanticDecl (IdentCon $2) $3 $5 $6 }

DataDecl :: { TopLevelDecl }         
  : data con '=' DataCon  ';'   { DataDecl (IdentCon $2) [] $4 }
  | data con DataParams '=' DataCon ';'  { DataDecl (IdentCon $2) $3 $5 }

DataCon :: { Constructor }
  : con                    { Constructor (IdentCon $1) [] }
  | con ADataCon           { Constructor (IdentCon $1) $2 }

ADataCon :: { [Type] }
  : AType ADataCon            { $1:$2 }
  | AType                     { [$1] }

DataParams :: { [Ident] }
  : ident DataParams         { (IdentVar $1):$2 }
  | ident                    { [IdentVar $1] }

FuncBind :: { TopLevelDecl }
  : ident FuncParams '=' Expr ';'   { FuncBindDecl (IdentVar $1) $2 $4 }
  | ident '=' Expr ';'              { FuncBindDecl (IdentVar $1) [] $3 }

FuncParams :: { [ Pattern ] }
  : AFuncParam FuncParams         { $1:$2 }
  | AFuncParam                    { [$1] }

AFuncParam :: { Pattern }
  : ident                         { IdentPattern $1 }
  | con                           { ConPattern $1 }
  | '(' BFuncParam ')'            { ParenPattern $2 }

BFuncParam :: { Pattern }
  : BFuncParam AFuncParam         { AppPattern $1 $2 }
  | AFuncParam                    { $1 }

FuncTypeDecl :: { TopLevelDecl }
  : ident '::' Type ';'      { FuncTypeDecl (IdentVar $1) $3 }

Type :: { Type }
  : BType                    { $1 }
  | BType '->' Type          { TypeFunc $1 $3 }

BType :: { Type }
  : AType BType              { TypeApp $1 $2 }
  | AType                    { $1 }

AType :: { Type }
  : con                      { TypeCon (IdentCon $1) }
  | ident                    { TypeVar (IdentVar $1) }
  | '(' Type ')'             { TypeParen $2 }

Expr :: { Exp }
  : Expr AExpr               { AppExp $1 $2 } 
  | AExpr                    { $1 }

AExpr :: { Exp }
  : LiteralExpr              { $1 }
  | LetExpr                  { $1 }
  | IfExpr                   { $1 }
  | LoopExpr                 { $1 }
  | '(' Expr ')'             { ParenExp $2 }
  | '(' Expr TupleEnd        { TupleExp ($2:$3) }
  | ident                    { IdentExp $1 }
  | con                      { ConsExp $1 }

LoopExpr :: { Exp }
  : loop ident while ident with Expr { LoopExp (IdentVar $2) (IdentVar $4) $6 }

IfExpr :: { Exp }
  : if Expr then Expr else Expr { IfExp $2 $4 $6 }

LetExpr :: { Exp }
  : let ident '=' Expr ';' LetExpr2     { LetExp (IdentVar $2) $4 $6 } 

LetExpr2 :: { Exp }
  : in Expr                             { $2 }
  | ident '=' Expr ';' LetExpr2         { LetExp (IdentVar $1) $3 $5 }

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