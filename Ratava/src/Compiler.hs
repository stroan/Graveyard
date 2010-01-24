module Compiler 
  ( compile
  ) where

import Types

compile (Ok _) = "Code to be generated :("
compile (Failed e) = "Compilation failed due to: " ++ e