-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Parser as P
import Interpreter as I

import Control.Monad.Error
import Text.ParserCombinators.Parsec

main :: IO ()
main = do a <- readFile "data/test.ppe"
          let a' = a ++ a
          putStrLn a'

runDocumentParserStr str = runParser P.documentParser () "[STRING]" str

runDocumentStr str = case runDocumentParserStr str of
                       Left e -> putStrLn $ "ERROR\n" ++ show e
                       Right ast -> do {a <- runErrorT (I.evalList ast);
                                       putStrLn $ show a }
