module Parser.Pretty
  ( prettyPrintAST
  ) where

import Data.List

import Types
import Parser.AST

prettyPrintAST :: EffectModule -> String
prettyPrintAST (EffectModule tlds) = intercalate "\n\n" (map prettyPrintTLD tlds)

prettyPrintTLD (FuncTypeDecl ident ty) = (getIdentStr ident) ++ " :: " ++ (prettyTypeString ty)
prettyPrintTLD (FuncBindDecl ident ps e) = (getIdentStr ident) ++ " " ++ (intercalate " " (map show ps)) ++ " =\n" ++ (prettyPrintExp e) ++ ";"
prettyPrintTLD (NewFuncBindDecl ident e) = (getIdentStr ident) ++ " =\n" ++ (prettyPrintExp e) ++ ";"
prettyPrintTLD a = show a

prettyPrintExp (LiteralExp l) = getLiteralStr l
prettyPrintExp (IdentExp i) = i
prettyPrintExp (ConsExp c) = c
prettyPrintExp (AppExp e e') = (prettyPrintExp e) ++ " " ++ (prettyPrintExp e')
prettyPrintExp (ParenExp e) = "(" ++ (prettyPrintExp e) ++ ")"
prettyPrintExp (LetExp i ps e e') = "let " ++ (getIdentStr i) ++ " "
                                    ++ (intercalate " " (map show ps)) ++ " = " ++ (prettyPrintExp e) 
                                    ++ ";\nin " ++ (prettyPrintExp e')
prettyPrintExp (IfExp c e e') = "if " ++ (prettyPrintExp c) ++ "\nthen " ++ (prettyPrintExp e) ++ "\nelse " ++ (prettyPrintExp e')
prettyPrintExp (LoopExp i i' e) = "loop " ++ (getIdentStr i) ++ " while " ++ (getIdentStr i') ++ " with " ++ (prettyPrintExp e)
prettyPrintExp (LambdaExp p e) = "(\\(" ++ (show p) ++ ") -> " ++ (prettyPrintExp e) ++ ")"