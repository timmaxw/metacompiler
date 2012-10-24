module Metacompiler.SL where

data Kind a
	= KindType a
	| KindFun a [Kind a] (Kind a)
	deriving (Eq, Show, Ord)

data Type a
	= TypeName a String
	| TypeApp a (Type a) (Type a)
	| TypeFun a [Type a] (Type a)
	| TypeLazy a (Type a)
	deriving (Eq, Show, Ord)

data Term a
	= TermName a String [Type a]
	| TermApp a (Term a) (Term a)
	| TermAbs a [(String, Type a)] (Term a)
	| TermCase a (Term a) [(String, [String], Term a)]
	| TermWrap a (Term a)
	| TermUnwrap a (Term a)
	deriving (Eq, Show, Ord)

-- `Dir` is short for "directive"
data Dir a
	= DirData {
		dirName :: String,
		dirTypeParams :: [(String, Kind a)],
		dirCtors :: [(String, [Type a])]
		}
	| DirLet {
		dirName :: String,
		dirTypeParams :: [(String, Kind a)],
		dirTermParams :: [(String, Type a)],
		dirType :: Type a,
		dirValue :: Term a
		}
	deriving (Eq, Show, Ord)

