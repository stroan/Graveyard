{
module Parser 
  ( parseTokens 
  ) where

import Types
}

%name parseTokens
%tokentype { Token }
%monad { ParseE }
%error { parseError }

%token
      data         { TokBuiltin "data" }
      '='          { TokEquals }
      '::'         { TokTypeSpec }
      '->'         { TokRArrow }
      string       { TokStringLit $$ }
      int          { TokIntLit $$ }
      real         { TokRealLit $$ }
      con          { TokConId $$ }
      ident        { TokIdentId $$ }

%%

Start :: { EffectModule } 
  : ModuleDecls              { EffectModule $1 }

ModuleDecls :: { [TopLevelDecl] }
  : TopLevelDecl ModuleDecls        { $1:$2 }
  | TopLevelDecl                    { [$1] }

TopLevelDecl :: { TopLevelDecl }
  : DataDecl                        { $1 }
  | FuncBind                        { $1 }

DataDecl :: { TopLevelDecl }         
  : data                     { DataDecl } 

FuncBind :: { TopLevelDecl }
  : FuncName '=' Expr        { FuncBind $1 [] $3 }

FuncName :: { Name }
  : ident                    { Name $1 }

Expr :: { Exp }
  : Expr AExpr               { AppExp $1 $2 } 
  | AExpr                    { $1 }

AExpr :: { Exp }
  : LiteralExpr              { $1 }

LiteralExpr :: { Exp }
  : string                   { LiteralExp (LiteralString $1) }
  | int                      { LiteralExp (LiteralInt $1) }
  | real                     { LiteralExp (LiteralReal $1) }


{ 
parseError _ = fail "parse error"
}