{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$up = [A-Z]

tokens :-

  $white+		;
  \-\-.*\n		;
  \{\-\-.*\-\-\}	;
  data			{ \s -> KW_data }
  $up$alpha*		{ \s -> ConId s }
  $alpha+		{ \s -> VarId s }
  =			{ \s -> Equals }

{
data Token = KW_data
           | KW_
     	     | ConId String
	         | VarId String
     	     | Equals
     	     | Strng String
	     deriving (Eq,Show)

scanTokens = alexScanTokens
}
