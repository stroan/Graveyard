{
module Lexer
  ( Token (..)
  , scanTokens
  ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$up = [A-Z]

tokens :-
  $white+               ;
  \-\-.*\n              ;
  \{\-\-.*\-\-\}        ;

  data                  { \_ -> KW_data }
  =                     { \_ -> Equals }
  ::                    { \_ -> TypeSpec }
  \-\>                  { \_ -> RArrow }

  \-?$digit+            { \s -> IntLit s }
  \?$digit+\.$digit+    { \s -> RealLit s }
  \".*\"                { \s -> StringLit s }

  $up$alpha*            { \s -> ConId s }
  $alpha+               { \s -> IdentId s }

{
data Token = KW_data
           | ConId String
           | IdentId String
           | Equals
           | TypeSpec
           | RArrow
           | StringLit String
           | IntLit String
           | RealLit String
           | Error
           deriving (Eq,Show)

safeScanTokens str = go ('\n',str)
  where go inp@(_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> [Error]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (take len str) : go inp'

scanTokens = safeScanTokens
}
