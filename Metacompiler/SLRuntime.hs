module SLRuntime where

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Ord)

newtype NameOfTerm = NameOfTerm { unNameOfTerm :: String } deriving (Eq, Ord)

newtype NameOfCtor = NameOfCtor { unNameOfCtor :: String } deriving (Eq, Show, Ord)

data Type = ...

data Term = ...

typeOfTerm :: Term -> Type
...

