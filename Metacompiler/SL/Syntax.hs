module Metacompiler.SL.Syntax where

import Data.Char

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Show, Ord)
newtype NameOfTerm = NameOfTerm { unNameOfTerm :: String } deriving (Eq, Show, Ord)

isValidTypeName :: String -> Bool
isValidTypeName "" = False
isValidTypeName (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c == '\'') xs

isValidTermName :: String -> Bool
isValidTermName "" = False
isValidTermName (x:xs) = isAlpha x && all (\c -> isAlphaNum c || c == '\'') xs

data Kind a
	= KindType a
	| KindFun a [Kind a] (Kind a)
	deriving (Eq, Show, Ord)

tagOfKind :: Kind a -> a
tagOfKind (KindType x) = x
tagOfKind (KindFun x _ _) = x

data Type a
	= TypeName a NameOfType
	| TypeApp a (Type a) (Type a)
	| TypeFun a [Type a] (Type a)
	| TypeLazy a (Type a)
	deriving (Eq, Show, Ord)

tagOfType :: Type a -> a
tagOfType (TypeName x _) = x
tagOfType (TypeApp x _ _) = x
tagOfType (TypeFun x _ _) = x
tagOfType (TypeLazy x _) = x

data Term a
	= TermName a NameOfTerm [Type a]
	| TermApp a (Term a) (Term a)
	| TermAbs a [(NameOfTerm, Type a)] (Term a)
	| TermCase a (Term a) [(NameOfTerm, [Type a], [NameOfTerm], Term a)]
	| TermWrap a (Term a)
	| TermUnwrap a (Term a)
	deriving (Eq, Show, Ord)

tagOfTerm :: Term a -> a
tagOfTerm (TermName x _ _) = x
tagOfTerm (TermApp x _ _) = x
tagOfTerm (TermAbs x _ _) = x
tagOfTerm (TermCase x _ _) = x
tagOfTerm (TermWrap x _) = x
tagOfTerm (TermUnwrap x _) = x

-- `Dir` is short for "directive"
data Dir a
	= DirData {
		tagOfDir :: a,
		nameOfDirData :: NameOfType,
		typeParamsOfDirData :: [(NameOfType, Kind a)],
		ctorsOfDirData :: [(NameOfTerm, [Type a])]
		}
	| DirLet {
		tagOfDir :: a,
		nameOfDirLet :: NameOfTerm,
		typeParamsOfDirLet :: [(NameOfType, Kind a)],
		termParamsOfDirLet :: [(NameOfTerm, Type a)],
		typeOfDirLet :: Type a,
		valueOfDirLet :: Term a
		}
	deriving (Eq, Show, Ord)

