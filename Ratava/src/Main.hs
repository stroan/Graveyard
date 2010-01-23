module Main (main) where

import Data.List
import Lexer
import Parser

testStrings = [ "str = \"str content\""
              , "reverse :: String -> String"
              , "Normalized x = Normalized x"
              , "piInt = 3"
              , "piReal = 3.1415"
              , "negPi = -3"]
              
testStrings' = [ "data" 
               , "func = 1 2 3" ]
              
runTest content = 
  intercalate "\n==>" resultLines
  where lexResult = scanTokens content
        parseResult = parseTokens lexResult
        resultLines = [content, show lexResult, show parseResult]

main = do
  mapM (putStrLn . runTest) testStrings'