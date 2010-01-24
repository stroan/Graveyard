{
module Lexer
  ( scanTokens
  ) where

import Types
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$up = [A-Z]

tokens :-
  $white+               ;
  \-\-.*\n              ;
  \{\-\-.*\-\-\}        ;

  data                  { \_ -> TokBuiltin "data" }
  =                     { \_ -> TokEquals }
  ::                    { \_ -> TokTypeSpec }
  \(                    { \_ -> TokOpenParen }
  \)                    { \_ -> TokCloseParen }
  \,                    { \_ -> TokComma }    
  \-\>                  { \_ -> TokRArrow }
  \;                    { \_ -> TokColon }

  \-?$digit+            { \s -> TokIntLit s }
  \?$digit+\.$digit+    { \s -> TokRealLit s }
  \".*\"                { \s -> TokStringLit s }

  $up$alpha*            { \s -> TokConId s }
  $alpha+               { \s -> TokIdentId s }

{

safeScanTokens str = go ('\n',str)
  where go inp@(_,str) =
          case alexScan inp 0 of
                AlexEOF -> []
                AlexError _ -> [TokError]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act (take len str) : go inp'

scanTokens = safeScanTokens
}
