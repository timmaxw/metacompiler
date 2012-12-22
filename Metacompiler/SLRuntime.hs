module SLRuntime where

newtype Var = Var String deriving (Eq, Ord)

data Type = ...

data Term = ...

typeOfTerm :: Term -> Type
...

