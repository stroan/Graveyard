module CmdLine
  ( CmdParameters(..)
  , parseArgs
  , isShowHelp, isDoParse
  ) where

import System

{-
-- Application parameter definitions
-}

data CmdParameters = CmdParamOutFile String
                   | CmdParamInFile String
                   | CmdParamParse
                   | CmdParamHelp
                   deriving (Show, Eq)

isSameParamType (CmdParamOutFile _) (CmdParamOutFile _) = True
isSameParamType (CmdParamInFile _) (CmdParamInFile _) = True
isSameParamType (CmdParamParse) (CmdParamParse) = True
isSameParamType (CmdParamHelp) (CmdParamHelp) = True
isSameParamType _ _ = False

isShowHelp (Just as) = (null as) || (not $ null $ filter (isSameParamType CmdParamHelp) as)
isShowHelp Nothing = True

isDoParse (Just as) = not $ null $ filter (isSameParamType CmdParamParse) as
isDoParse Nothing = False


cmdOptions = [ CmdLineOption (Just "o") 2 (\[_,f] -> CmdParamOutFile f)
             , CmdLineOption (Just "p") 1 (\_ -> CmdParamParse)
             , CmdLineOption (Just "h") 1 (\_ -> CmdParamHelp)
             , CmdLineOption Nothing  1 (\[s] -> CmdParamInFile s) ]


parseArgs :: IO (Maybe [CmdParameters])
parseArgs = do
  args <- getArgs
  return $ doArgParse cmdOptions args []

{-
-- Backend for the command line argument parsing system.
-}

data CmdLineOption a = CmdLineOption { optionPrefix :: Maybe String
				     , optionParams :: Int
                                     , optionFunc   :: ([String] -> a) }

optionMatches :: String -> CmdLineOption a -> Bool
optionMatches str (CmdLineOption (Just a) _ _) = str == ("-" ++ a)
optionMatches ('-':ss) (CmdLineOption Nothing _ _) = False
optionMatches _ (CmdLineOption Nothing _ _) = True

doArgParse :: [CmdLineOption a] -> [String] -> [a] -> Maybe [a]
doArgParse _ [] outs = Just outs
doArgParse opts args outs =
  let matchingOpts = filter (optionMatches (head args)) opts
      matchingOpt = head matchingOpts
      numArgs = (optionParams matchingOpt)
      a = (optionFunc matchingOpt) (take numArgs args)
  in if (null matchingOpts) || (numArgs > (length args))
        then Nothing
        else doArgParse opts (drop numArgs args) (outs ++ [a])

  

