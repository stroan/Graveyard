module CompilerM 
  ( CompilerM (..)
  , fromCompilerM, wasCompSuccess, fromCompilerME
  ) where

data CompilerM a = CompErr String 
                 | CompSuccess a 
                 deriving (Show, Eq)


wasCompSuccess (CompSuccess a) = True
wasCompSuccess _ = False
fromCompilerM (CompSuccess a) = a
fromCompilerME (CompErr s) = s

instance Monad CompilerM where
  return = CompSuccess
  (CompErr s) >>= g = CompErr s
  (CompSuccess a) >>= g = g a
  fail a = CompErr a

instance Functor CompilerM where
  f `fmap` (CompErr s) = CompErr s
  f `fmap` (CompSuccess a) = CompSuccess (f a)

