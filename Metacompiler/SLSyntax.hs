module Metacompiler.SLSyntax where

import Data.Char

newtype NameOfType = NameOfType { unNameOfType :: String } deriving (Eq, Show, Ord)
newtype NameOfCtor = NameOfCtor { unNameOfCtor :: String } deriving (Eq, Show, Ord)
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

data Type a
	= TypeName a NameOfType
	| TypeApp a (Type a) (Type a)
	| TypeFun a [Type a] (Type a)
	| TypeLazy a (Type a)
	deriving (Eq, Show, Ord)

data Term a
	= TermName a NameOfTerm [Type a]
	| TermApp a (Term a) (Term a)
	| TermAbs a [(NameOfTerm, Type a)] (Term a)
	| TermCase a (Term a) [(NameOfCtor, [Type a], [NameOfTerm], Term a)]
	| TermWrap a (Term a)
	| TermUnwrap a (Term a)
	deriving (Eq, Show, Ord)

-- `Dir` is short for "directive"
data Dir a
	= DirData {
		dirNameOfType :: NameOfType,
		dirTypeParams :: [(NameOfType, Kind a)],
		dirCtors :: [(NameOfCtor, [Type a])]
		}
	| DirLet {
		dirNameOfTerm :: NameOfTerm,
		dirTypeParams :: [(NameOfType, Kind a)],
		dirTermParams :: [(NameOfTerm, Type a)],
		dirType :: Type a,
		dirValue :: Term a
		}
	deriving (Eq, Show, Ord)

