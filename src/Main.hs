module Main (main) where

import Data.List
import CmdLine
import System
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
    
doCompile content backend = do
  let lexResult = scanTokens content
  let parseResult = parseTokens lexResult
  let lexResult' = scanTokens backend
  let parseResult' = parseTokens lexResult'
  if isParseEOk parseResult
    then if isParseEOk parseResult' 
	    then do let (EffectModule tlds) = getParse parseResult
			(EffectModule tlds') = getParse parseResult'
			tc = do t <- typeCheck (EffectModule $ tlds ++ tlds')
				c <- compile t
				return (t,c)
		    if wasCompSuccess tc
		      then do (_,s) <- return $ fromCompilerM tc
			      putStrLn s
		      else putStrLn $ fromCompilerME tc
	    else putStrLn $ getParseE parseResult'
    else putStrLn $ getParseE parseResult
  where
	isParseEOk (Ok _) = True
	isParseEOk _ = False
        getParse (Ok x) = x
	getParse _ = undefined
	getParseE (Failed s) = s
	getParseE _ = undefined


doParse = undefined

doShowHelp = do
  putStrLn "Citten 0.2.0"
  putStrLn "Citten [-h] [-p] [-o <out>] <files>"
  putStrLn "-h\tDisplay help."
  putStrLn "-p\tOnly parse code files."

doMain args
  | isShowHelp args = doShowHelp
  | isDoParse args  = doParse args

main = do
  args <- parseArgs
  doMain args
  --args <- getArgs
  --putStrLn $ show args ++ "\n"
  --backend <- readFile (args !! 1)
  --content <- readFile (args !! 0)
  --runTest' (content ++ backend)