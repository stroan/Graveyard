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
$up = [A-Z]
$special   = [\(\)\,\;\[\]\`\{\}]
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$graphic = [$alpha $digit $special $ascsymbol]

@string  = $graphic # [\"\\] | " "

tokens :-
  $white+                    ;
  \-\- .*                    ;

  data                  { \p _ -> mT p $ TokBuiltin "data" }
  basetype              { \p _ -> mT p $ TokBuiltin "basetype" }
  basefunc              { \p _ -> mT p $ TokBuiltin "basefunc" }
  semantic              { \p _ -> mT p $ TokBuiltin "semantic" }
  =                     { \p _ -> mT p $ TokEquals }
  ::                    { \p _ -> mT p $ TokTypeSpec }
  \(                    { \p _ -> mT p $ TokOpenParen }
  \)                    { \p _ -> mT p $ TokCloseParen }
  \,                    { \p _ -> mT p $ TokComma }    
  \-\>                  { \p _ -> mT p $ TokRArrow }
  \;                    { \p _ -> mT p $ TokColon }

  \-?$digit+            { \p s -> mT p $ TokIntLit s }
  \?$digit+\.$digit+    { \p s -> mT p $ TokRealLit s }
  \" @string* \"        { \p s -> mT p $ TokStringLit s }

  $up$alpha*            { \p s -> mT p $ TokConId s }
  $alpha+               { \p s -> mT p $ TokIdentId s }

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
