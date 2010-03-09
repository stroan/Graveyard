module Main (main) where

import Data.List
import Lexer
import Parser
import Compiler
import TypeChecker
import Types

runTest' content = do
  let lexResult = scanTokens content
  putStrLn $ show lexResult
  putStrLn "---------------"
  let parseResult = parseTokens lexResult
  putStrLn $ show parseResult
  putStrLn "---------------"
  let tc = do { t <- typeCheck (getParse parseResult); c <- compile t; return (t,c) }
  putStrLn $ show tc
  putStrLn "---------------"
  if wasCompSuccess tc
    then do (_,s) <- return $ fromCompilerM tc
	    putStrLn s
    else return ()
  where
	isParseEOk (Ok _) = True
	isParseEOk _ = False
        getParse (Ok x) = x
	getParse _ = undefined
     
runTest content = do
  let lexResult = scanTokens content
      parseResult = parseTokens lexResult
  if isParseEOk parseResult
    then do t <- typeCheck (getParse parseResult)
	    c <- compile t
	    return $ intercalate "\n==>\n" [ content
					   , show lexResult
					   , show parseResult
					   , show t, show c ]
    else return ""
  where
	isParseEOk (Ok _) = True
	isParseEOk _ = False
        getParse (Ok x) = x
	getParse _ = undefined

main = do
  content <- readFile "test/test1.rtv"
  runTest' content