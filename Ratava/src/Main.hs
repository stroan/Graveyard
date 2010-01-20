import Lexer

testStrings = [ "str = \"str content\""
              , "reverse :: String -> String"
              , "Normalized x = Normalized x"
              , "piInt = 3"
              , "piReal = 3.1415"
              , "negPi = -3"]
              
runTest content = content ++ "\n==> " ++ show (scanTokens content) ++ "\n"

main = do
  mapM (putStrLn . runTest) testStrings