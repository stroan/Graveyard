module Main (main) where

import Data.List
import Lexer
import Parser
import Compiler

testStrings = [ "str = \"str content\" ="
              , "reverse :: String -> String"
              , "Normalized x = Normalized x"
              , "piInt = 3"
              , "piReal = 3.1415"
              , "negPi = -3"]
              
testStrings' = [ "func :: (MyType a -> Bool) -> Char;" 
               , "func x = add (add 1 2) 2;"
               , "data Foo a = Bar Tree String;"
               , "basetype Double \"double\" = Double Real \"[[0]]\";"
               , "basefunc mulDD :: Double -> Double -> Double \"mul([[0]],[[0]])\";"
               , "semantic Position a = Position a \"POSITION\";" ]
              
runTest content = 
  intercalate "\n==>\n" resultLines
  where lexResult = scanTokens content
        parseResult = parseTokens lexResult
        compileResult = compile parseResult
        resultLines = [content, show lexResult, show parseResult, compileResult]

main = do
  content <- readFile "test/test1.rtv"
  putStrLn $ runTest content