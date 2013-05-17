module Metacompiler.SLRuntime.Types where

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Show, Ord)
newtype NameOfTerm = NameOfTerm { unNameOfTerm :: String } deriving (Eq, Show, Ord)

data Kind
	= KindType
	| KindFun Kind Kind
	deriving (Show, Eq)

data DataDefn = DataDefn {
	nameOfDataDefn :: NameOfType,
	typeParamsDataDefn :: [Kind]
	} deriving Eq

data CtorDefn = CtorDefn {
	nameOfCtorDefn :: NameOfTerm,
	parentDataOfCtorDefn :: SLDataDefn,
	fieldTypesOfCtorDefn :: [[Type] -> Type]
	}

data TermDefn = TermDefn {
	nameOfTermDefn :: NameOfTerm,
	typeParamsOfTermDefn :: [Kind],
	typeOfTermDefn :: [Type] -> Type,
	valueOfTermDefn :: [Type] -> Term
	}

data Type
	= TypeDefined DataDefn
	| TypeName NameOfType Kind
	| TypeApp Type Type
	| TypeFun Type Type
	| TypeLazy Type
	deriving Eq

data Term
	= TermDefined TermDefn [Type]
	| TermName NameOfTerm Type
	| TermApp Term Term
	| TermAbs (NameOfTerm, Type) Term
	| TermCase Term [(CtorDefn, [Type], [NameOfTerm], Term)]
	| TermData CtorDefn [Type] [Term]
	| TermWrap Term
	| TermUnwrap Term
