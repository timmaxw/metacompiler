module TestMetacompiler.Error where

import Metacompiler.Error

assumeSuccess :: String -> ErrorMonad a -> a
assumeSuccess _ (Success x) = x
assumeSuccess msg1 (Failure msg2) = error (msg1 ++ ": " ++ msg2)

