{
module Lexer
  ( scanTokens
  ) where

import Types
import Data.List
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphanum = [$digit $alpha]
$up = [A-Z]
$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\_]
$graphic = [$alpha $digit $special $ascsymbol]

@string  = $graphic # [\"\\] | " "

tokens :-
  $white+                    ;
  \-\- .*                    ;

  data                  { \p _ -> mT p $ TokBuiltin "data" }
  basetype              { \p _ -> mT p $ TokBuiltin "basetype" }
  basefunc              { \p _ -> mT p $ TokBuiltin "basefunc" }
  semantic              { \p _ -> mT p $ TokBuiltin "semantic" }
  parameter		{ \p _ -> mT p $ TokBuiltin "parameter" }
  let                   { \p _ -> mT p $ TokBuiltin "let" }
  in                    { \p _ -> mT p $ TokBuiltin "in" }
  if                    { \p _ -> mT p $ TokBuiltin "if" }
  then                  { \p _ -> mT p $ TokBuiltin "then" }
  else                  { \p _ -> mT p $ TokBuiltin "else" }
  loop			{ \p _ -> mT p $ TokBuiltin "loop" }
  while			{ \p _ -> mT p $ TokBuiltin "while" }
  with			{ \p _ -> mT p $ TokBuiltin "with" }
  technique             { \p _ -> mT p $ TokBuiltin "technique" }
  =                     { \p _ -> mT p $ TokEquals }
  ::                    { \p _ -> mT p $ TokTypeSpec }
  \(                    { \p _ -> mT p $ TokOpenParen }
  \)                    { \p _ -> mT p $ TokCloseParen }
  \,                    { \p _ -> mT p $ TokComma }    
  \-\>                  { \p _ -> mT p $ TokRArrow }
  \;                    { \p _ -> mT p $ TokColon }

  \-?$digit+\.$digit+    { \p s -> mT p $ TokRealLit s }
  \-?$digit+            { \p s -> mT p $ TokIntLit s }
  \" @string* \"        { \p s -> mT p $ TokStringLit (take ((length s) - 2) (tail s)) }

  $up$alphanum*            { \p s -> mT p $ TokConId s }
  $alpha$alphanum*         { \p s -> mT p $ TokIdentId s }

{

mT (AlexPn _ l c) tok = Lexeme l c tok

safeScanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> [mT pos TokEOF]
                AlexError _ -> [mT pos TokError]
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> act pos (take len str) : go inp'

scanTokens = safeScanTokens
    
}
