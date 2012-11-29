module CmdLine
  ( CmdParameter(..)
  , parseArgs
  , isShowHelp, isDoParse
  , getOutFile, getSourceFiles
  ) where

import System

{-----------------
-- Application parameter definitions.
--  This section of the file contains the
--  types and functions which are exposed
--  in this module.
-----------------}

data CmdParameter = CmdParamOutFile String
                   | CmdParamInFile String
                   | CmdParamParse
                   | CmdParamHelp
                   deriving (Show, Eq)

isSameParamType :: CmdParameter -- Parameter 1
                -> CmdParameter -- Parameter 2
                -> Bool         -- True if they have the same constructor.
isSameParamType (CmdParamOutFile _) (CmdParamOutFile _) = True
isSameParamType (CmdParamInFile _) (CmdParamInFile _) = True
isSameParamType (CmdParamParse) (CmdParamParse) = True
isSameParamType (CmdParamHelp) (CmdParamHelp) = True
isSameParamType _ _ = False

getOutFileName :: CmdParameter  -- Parameter (Must be CmdParamOutFile)
               -> String        -- Filename
getOutFileName (CmdParamOutFile name) = name

getInFileName :: CmdParameter  -- Parameter (Must be CmdParamInFile)
              -> String        -- Filename
getInFileName (CmdParamInFile name) = name

isShowHelp :: Maybe [CmdParameter]  -- Parsed parameters
           -> Bool                  -- Do set of parameters imply showing the help.
isShowHelp (Just as) = (null as) || (not $ null $ filter (isSameParamType CmdParamHelp) as)
isShowHelp Nothing = True

isDoParse :: Maybe [CmdParameter]  -- Parsed parameters
          -> Bool                  -- Do set of paameters imply only parsing
isDoParse (Just as) = not $ null $ filter (isSameParamType CmdParamParse) as
isDoParse Nothing = False

getOutFile :: Maybe [CmdParameter]  -- Parsed parameters
           -> Maybe String          -- The specified output file, if any.
getOutFile (Just as) = 
  let out = map getOutFileName $ filter (isSameParamType (CmdParamOutFile "")) as
  in if null out
       then Nothing
       else Just $ head out
getOutFile Nothing = Nothing

getSourceFiles :: Maybe [CmdParameter] -- Parsed parameters
               -> [String]
getSourceFiles (Just as) = map getInFileName $ filter (isSameParamType (CmdParamInFile "")) as
getSourceFiles Nothing = []

cmdOptions :: [CmdLineOption CmdParameter]          -- Program's command line options.
cmdOptions = [ CmdLineOption (Just "o") 2 (\[_,f] -> CmdParamOutFile f)
             , CmdLineOption (Just "p") 1 (\_ -> CmdParamParse)
             , CmdLineOption (Just "h") 1 (\_ -> CmdParamHelp)
             , CmdLineOption Nothing  1 (\[s] -> CmdParamInFile s) ]

parseArgs :: IO (Maybe [CmdParameter]) -- Return the set of parsed parameters.
parseArgs = do
  args <- getArgs
  return $ doArgParse cmdOptions args []

{------------------------
-- Backend for the command line argument parsing system.
--  These types and functions are not exposed. They provide
--  the infrastructure for parsing and managing command
--  line parameters.
------------------------}

data CmdLineOption a = CmdLineOption { optionPrefix :: Maybe String
				     , optionParams :: Int
                                     , optionFunc   :: ([String] -> a) }

optionMatches :: String          -- String associated with the current arg.
              -> CmdLineOption a -- Current option to check against
              -> Bool            -- Whether it was a match
optionMatches str (CmdLineOption (Just a) _ _) = str == ("-" ++ a)
optionMatches ('-':ss) (CmdLineOption Nothing _ _) = False
optionMatches _ (CmdLineOption Nothing _ _) = True

doArgParse :: [CmdLineOption a]  -- Set of command line option definitions.
           -> [String]           -- List of arguments
           -> [a]                -- Current list of parsed parameters.
           -> Maybe [a]          -- Output parameters.
doArgParse _ [] outs = Just outs
doArgParse opts args outs =
  let matchingOpts = filter (optionMatches (head args)) opts
      matchingOpt = head matchingOpts
      numArgs = (optionParams matchingOpt)
      a = (optionFunc matchingOpt) (take numArgs args)
  in if (null matchingOpts) || (numArgs > (length args))
        then Nothing
        else doArgParse opts (drop numArgs args) (outs ++ [a])

  

