module Parser.Pretty
  ( prettyPrintAST
  ) where

import Data.List

import Parser.AST

prettyPrintAST :: EffectModule -> String
prettyPrintAST (EffectModule tlds) = intercalate "\n\n" (map show tlds)