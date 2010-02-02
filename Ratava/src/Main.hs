module Main (main) where

import Data.List
import Lexer
import Parser
import Compiler
              
runTest content = 
  intercalate "\n==>\n" resultLines
  where lexResult = scanTokens content
        parseResult = parseTokens lexResult
        compileResult = compile parseResult
        resultLines = [content, show lexResult, show parseResult, compileResult]

main = do
  content <- readFile "test/test1.rtv"
  putStrLn $ runTest content