module SLRuntime where

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Ord)

newtype NameOfTerm = NameOfTerm { unNameOfTerm :: String } deriving (Eq, Ord)

newtype NameOfCtor = NameOfCtor { unNameOfCtor :: String } deriving (Eq, Show, Ord)

data Kind
	= KindType
	| KindFun Kind Kind

data Type
	= TypeName NameOfType Kind
	| TypeApp Type Type
	| TypeFun Type Type
	| TypeLazy Type

data Term
	= TermName NameOfTerm [Type] Type
	| TermApp Term Term
	| TermAbs (NameOfTerm, Type) Term
	| TermCase Term [(NameOfCtor, [NameOfTerm], Term)]
	| TermWrap Term
	| TermUnwrap Term

kindOfType :: Type -> Kind
...

typeOfTerm :: Term -> Type
...

