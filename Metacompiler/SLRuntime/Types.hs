module Metacompiler.SLRuntime.Types where

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Ord)
instance Show NameOfType where
	show (NameOfType n) = "NameOfType " ++ show n

newtype NameOfTerm = NameOfTerm { unNameOfTerm :: String } deriving (Eq, Ord)
instance Show NameOfTerm where
	show (NameOfTerm n) = "NameOfTerm " ++ show n

data Kind
	= KindType
	| KindFun Kind Kind
	deriving (Show, Eq)

data DataDefn = DataDefn {
	nameOfDataDefn :: NameOfType,
	typeParamsOfDataDefn :: [Kind]
	} deriving Eq
instance Show DataDefn where
	show (DataDefn name _) = "(DataDefn (" ++ show name ++ ") ...)"

data CtorDefn = CtorDefn {
	nameOfCtorDefn :: NameOfTerm,
	parentDataOfCtorDefn :: DataDefn,
	fieldTypesOfCtorDefn :: [[Type] -> Type]
	}
instance Show CtorDefn where
	show (CtorDefn name _ _) = "(CtorDefn (" ++ show name ++ ") ...)"

data TermDefn = TermDefn {
	nameOfTermDefn :: NameOfTerm,
	typeParamsOfTermDefn :: [Kind],
	typeOfTermDefn :: [Type] -> Type,
	valueOfTermDefn :: [Type] -> Term
	}
instance Show TermDefn where
	show (TermDefn name _ _ _) = "(TermDefn (" ++ show name ++ ") ...)"

data Type
	= TypeDefined DataDefn
	| TypeName NameOfType Kind
	| TypeApp Type Type
	| TypeFun Type Type
	| TypeLazy Type
	deriving (Eq, Show)

data Term
	= TermDefined TermDefn [Type]
	| TermName NameOfTerm Type
	| TermApp Term Term
	| TermAbs (NameOfTerm, Type) Term
	| TermCase Term [(CtorDefn, [Type], [NameOfTerm], Term)]
	| TermData CtorDefn [Type] [Term]
	| TermWrap Term
	| TermUnwrap Term
	deriving Show
