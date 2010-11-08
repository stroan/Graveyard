-----------------------------------------------------------------------------
--
-- Module      :  Parser
-- Copyright   :  Stephen Roantree
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Parser (
    documentParser
) where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Maybe

import Parser.AST

--
-- Helper functions
--
choiceStrings :: [String] -> CharParser st String
choiceStrings a = choice $ map (try  . string) a

tryChoices :: [CharParser st a] -> CharParser st a
tryChoices a = choice (map try a)

applyNumSign NumNeg num = - num
applyNumSign _ num = num

addPosition :: CharParser st (SourcePos -> a) -> CharParser st a
addPosition a = do pos <- getPosition
                   v <- a
                   return $ v pos


--
-- Lexer functions
--
delimiter :: CharParser st Char
delimiter = whitespace <|> oneOf "()\";"

whitespace :: CharParser st Char
whitespace = oneOf "\n "

comment :: CharParser st String
comment = char ';' >> manyTill anyChar (char '\n')

atmosphere :: CharParser st String
atmosphere = do {a <- whitespace; return [a]} <|> comment

interspaceToken :: CharParser st String
interspaceToken = liftM (foldr (++) []) $ many atmosphere


identifier :: CharParser st String
identifier = stdIdentifier <|> peculiarIdentifier
             where stdIdentifier = do a <- initial
                                      b <- many subsequent
                                      return $ a:b

initial :: CharParser st Char
initial = letter <|> specialInitial

specialInitial :: CharParser st Char
specialInitial = oneOf "\"!$%&*/:<=>?^_"

subsequent :: CharParser st Char
subsequent = initial <|> digit <|> specialSubsequent

specialSubsequent :: CharParser st Char
specialSubsequent = oneOf "+-.@"

peculiarIdentifier :: CharParser st String
peculiarIdentifier = (try $ string "...") <|> (liftM (\x -> [x]) $ oneOf "+-")

variable :: CharParser st String
variable = identifier

boolean :: CharParser st Bool
boolean = true <|> false
          where true  = try $ do { string "#t"; return True }
                false = try $ do { string "#f"; return False }

character :: CharParser st Char
character = do string "#\\"
               getSpace <|> getNewline <|> anyChar
               where getSpace   = try $ do { string "space"; return ' ' }
                     getNewline = try $ do { string "newline"; return '\n' }

stringLit :: CharParser st String
stringLit = do char '\"'
               a <- many stringElement
               char '\"'
               return a

stringElement :: CharParser st Char
stringElement = noneOf "\"\\"

number :: CharParser st NumToken
number = num Base10

num :: NumBases -> CharParser st NumToken
num a = do numPrefix a
           real <- numReal a
           return $ NumReal real


numReal a = do sign <- numSign
               doDecimal sign <|> doUInt sign
            where doUInt s    = try $ do {i <- numUInteger a s;
                                          return $ NumRealInteger i}
                  doDecimal s = try $ do {i <- numDecimal a s;
                                          return $ NumRealDecimal i}

numDecimal :: NumBases -> NumSign -> CharParser st Float
numDecimal Base10 s = do pre <- many1 $ numDigit Base10
                         char '.'
                         post <- many1 $ numDigit Base10
                         return $ applyNumSign s $ read (pre ++ "." ++ post)

numUInteger :: NumBases -> NumSign -> CharParser st Integer
numUInteger a s = do d <- many1 (numDigit a)
                     return $ applyNumSign s $ readBase a d
                  where readBase Base10 = read

numSign :: CharParser st NumSign
numSign = (char '+' >> return NumPos)
          <|> (char '-' >> return NumNeg)
          <|> (return NumPos)

numPrefix :: NumBases -> CharParser st ()
numPrefix = numRadix

numRadix :: NumBases -> CharParser st ()
numRadix Base10 = try (string "#d" >> return ()) <|> return ()

numDigit :: NumBases -> CharParser st Char
numDigit Base10 = digit

--
-- Parser functions
--
expression :: CharParser st (Expression SourcePos)
expression = exprLiteral <|>
             exprVariable <|>
             exprQuote <|>
             do { char '(';
                  a <- (try exprList) <|> exprDotList;
                  char ')';
                  return a
                }

exprVariable :: CharParser st (Expression SourcePos)
exprVariable = addPosition $ do {v <- variable;
                                 return $ ExprVariable v}

exprLiteral :: CharParser st (Expression SourcePos)
exprLiteral = addPosition $ do {d <- choice [exprSelfEvaluating];
                                return $ ExprLiteral d}

exprSelfEvaluating :: CharParser st Literal
exprSelfEvaluating = tryChoices $  [makeLit (boolean, LiteralBool),
                                    makeLit (number, LiteralNum),
                                    makeLit (character, LiteralChar),
                                    makeLit (stringLit, LiteralString)]
                     where makeLit (p,c) = do l <- p
                                              return $ c l

exprQuote :: CharParser st (Expression SourcePos)
exprQuote = addPosition $ do {char '\'';
                              body <- expression;
                              return $ (\p -> ExprList [ExprVariable "quote" p,body] p)
                             }

exprList :: CharParser st (Expression SourcePos)
exprList = addPosition $ do { a <- sepBy expression interspaceToken;
                              return $ ExprList a }

exprDotList :: CharParser st (Expression SourcePos)
exprDotList = addPosition $ do {a <- endBy expression whitespace;
                                char '.'; whitespace;
                                b <- expression;
                                return $ ExprDotList (a ++ [b])}

documentParser :: CharParser st [Expression SourcePos]
documentParser = do pos <- getPosition
                    es <- manyTill ( (comment >> return Nothing) <|>
                                     (whitespace >> return Nothing) <|>
                                     (do { e <- expression; return $ Just e }) ) eof
                    let es' = filter isJust es
                    return $ (map fromJust es')


