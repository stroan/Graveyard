module Main (main) where

import Data.List
import System
import System.IO

import CmdLine
import CodeGenerator
import CompilerM
import Lexer.Lexer
import Parser.Parser
import Parser.Pretty
import TypeChecker.TypeChecker

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

getOutFileHandle (Just n) = openFile n WriteMode
getOutFileHandle Nothing = return stdout

getInFilesSafe args = do
  let srcFiles = getSourceFiles args
  if null srcFiles
     then fail "No input files"
     else return srcFiles

parseFile file = do
  contents <- readFile file
  let t = scanTokens contents
  return $ parseTokens t

printParseResult :: Handle -> (String, ParseE EffectModule) -> IO ()
printParseResult ho (name, Ok e) = do
  hPutStrLn ho "======="
  hPutStrLn ho ("Parse output for " ++ name)
  hPutStrLn ho (prettyPrintAST e)

printParseResult ho (name, Failed s) = do
  hPutStrLn ho "======="
  hPutStrLn ho ("Error parsing " ++ name)
  hPutStrLn ho ("\t" ++ s)
  hPutStrLn ho "======="

doParse :: Maybe [CmdParameter] -> IO ()
doParse args = do
  ho <- getOutFileHandle (getOutFile args)
  srcFiles <- getInFilesSafe args
  srcParsed <- mapM parseFile srcFiles
  mapM_ (printParseResult ho) (zip srcFiles srcParsed)

doShowHelp :: IO ()
doShowHelp = do
  putStrLn "Citten 0.2.0"
  putStrLn "Citten [-h] [-p] [-o <out>] <files>"
  putStrLn "-h\tDisplay help."
  putStrLn "-p\tOnly parse code files."

doMain :: Maybe [CmdParameter] -> IO ()
doMain args
  | isShowHelp args = doShowHelp
  | isDoParse args  = doParse args
  | otherwise       = undefined

main = do
  args <- parseArgs
  doMain args