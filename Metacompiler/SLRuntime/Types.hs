module Metacompiler.SLRuntime.Types where

newtype NameOfSLType = NameOfSLType { unNameOfSLType :: String } deriving (Eq, Show, Ord)
newtype NameOfSLTerm = NameOfSLTerm { unNameOfSLTerm :: String } deriving (Eq, Show, Ord)

data SLKind
	= SLKindType
	| SLKindFun SLKind SLKind
	deriving (Show, Eq)

data SLDataDefn = SLDataDefn {
	nameOfSLDataDefn :: NameOfSLType,
	typeParamsOfSLDataDefn :: [SLKind]
	}

data SLCtorDefn = SLCtorDefn {
	nameOfSLCtorDefn :: NameOfSLTerm,
	parentDataOfSLCtorDefn :: SLDataDefn,
	fieldTypesOfSLCtorDefn :: [[SLType] -> SLType]
	}

data SLTermDefn = SLTermDefn {
	nameOfSLTermDefn :: NameOfSLTerm,
	typeParamsOfSLTermDefn :: [SLKind],
	typeOfSLTermDefn :: [SLType] -> SLType,
	valueOfSLTermDefn :: [SLType] -> SLTerm
	}

data SLType
	= SLTypeDefined SLDataDefn
	| SLTypeName NameOfSLType SLKind
	| SLTypeApp SLType SLType
	| SLTypeFun SLType SLType
	| SLTypeLazy SLType

data SLTerm
	= SLTermDefined SLTermDefn [SLType]
	| SLTermName NameOfSLTerm SLType
	| SLTermApp SLTerm SLTerm
	| SLTermAbs (NameOfSLTerm, SLType) SLTerm
	| SLTermCase SLTerm [(SLCtorDefn, [SLType], [NameOfSLTerm], SLTerm)]
	| SLTermData SLCtorDefn [SLType] [SLTerm]
	| SLTermWrap SLTerm
	| SLTermUnwrap SLTerm
