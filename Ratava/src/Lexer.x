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
$special   = [\(\)\,\;\[\]\`\{\}]
$graphic = [$alpha $digit $special]

@string  = $graphic # [\"\\] | " "

tokens :-
  $white+                    ;
  \-\- @string*              ;
  \{\-\- @string* \-\-\}     ;

  data                  { \_ -> TokBuiltin "data" }
  basetype              { \_ -> TokBuiltin "basetype" }
  basefunc              { \_ -> TokBuiltin "basefunc" }
  semantic              { \_ -> TokBuiltin "semantic" }
  =                     { \_ -> TokEquals }
  ::                    { \_ -> TokTypeSpec }
  \(                    { \_ -> TokOpenParen }
  \)                    { \_ -> TokCloseParen }
  \,                    { \_ -> TokComma }    
  \-\>                  { \_ -> TokRArrow }
  \;                    { \_ -> TokColon }

  \-?$digit+            { \s -> TokIntLit s }
  \?$digit+\.$digit+    { \s -> TokRealLit s }
  \" @string* \"                { \s -> TokStringLit s }

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
