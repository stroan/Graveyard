module Main (main) where

import Data.List
import System
import System.IO

import CmdLine
import CodeGenerator
import CompilerM
import Lexer.Lexer
import Parser.AST
import Parser.Parser
import Parser.Pretty
import TypeChecker.TypeChecker

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

parseFileSafe file = do
  contents <- readFile file
  let t = scanTokens contents
      e = parseTokens t
  if parseSuccess e
     then return (getParseResult e)
     else do hPutStrLn stderr (getParseError e)
             fail "Parse error"

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

doCompile' :: Maybe [CmdParameter] -> IO ()
doCompile' args = do
  ho <- getOutFileHandle (getOutFile args)
  srcFiles <- getInFilesSafe args
  srcParsed <- mapM parseFileSafe srcFiles
  let m = foldl mergeModules (head srcParsed) (tail srcParsed)
      compiled = do { a <- typeCheck m; compile a }
  if wasCompSuccess compiled
     then do hPutStr ho (fromCompilerM compiled)
             hClose ho
     else do hPutStrLn stderr (fromCompilerME compiled)
             fail "Compilation error"

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
  | otherwise       = doCompile' args

main = do
  args <- parseArgs
  doMain args