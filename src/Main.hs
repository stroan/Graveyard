module Main (main) where

import Data.List
import Lexer
import Parser
import Compiler
import TypeChecker
import Types
              
runTest content = 
  intercalate "\n==>\n" resultLines
  where lexResult = scanTokens content
        parseResult = parseTokens lexResult
        typeResult = if isParseEOk parseResult
			then typeCheck (getParse parseResult)
			else return ""
        resultLines = [content, show lexResult, show parseResult, show typeResult]
	isParseEOk (Ok _) = True
	isParseEOk _ = False
        getParse (Ok x) = x
	getParse _ = undefined

main = do
  content <- readFile "test/test1.rtv"
  putStrLn $ runTest content