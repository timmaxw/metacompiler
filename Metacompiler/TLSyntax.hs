module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Metacompiler.SLSyntax as SL

-- `MetaType` represents a translation-language meta-type. It is more of a
-- syntactic representation than a semantic one; for example, it doesn't
-- represent `fun ... -> ...` blocks with multiple arguments down into multiple
-- single-argument blocks. It is parameterized on a "tag" type, which will
-- typically be `Range`.

data MetaType a
	= MTSLType {
		tagOfMetaType :: a
	}
	| MTSLTerm {
		tagOfMetaType :: a,
		typeOfMetaType :: MetaObject a
	}
	| MTJSExprType {
		tagOfMetaType :: a
		slEquivOfMetaType :: MetaObject a
	}
	| MTJSExpr {
		tagOfMetaType :: a,
		slEquivOfMetaType :: MetaObject a,
		typeOfMetaType :: MetaObject a
	}
	| MTFun {
		tagOfMetaType :: a,
		paramsOfMetaType :: [(String, MetaType a)],
		resultOfMetaType :: MetaType a
	}
	deriving Show

-- `MetaObject` represents a translation-language meta-object.

data MetaObject a
	= MOApp {
		tagOfMetaObject :: a,
		funOfMetaObject :: MetaObject a,
		argOfMetaObject :: MetaObject a
	}
	| MOAbs {
		tagOfMetaObject :: a,
		paramsOfMetaObject :: [(String, MetaType a)],
		resultOfMetaObject :: MetaObject a
	}
	| MOVar {
		tagOfMetaObject :: a,
		varOfMetaObject :: String
	}
	| MOSLTypeLiteral {
		tagOfMetaObject :: a,
		slTypeLiteralOfMetaObject :: SL.Type a,
		slTypeBindsOfMetaObject :: [(String, MetaObject a)]
	}
	| MOSLTermLiteral {
		tagOfMetaObject :: a,
		slTermLiteralOfMetaObject :: SL.Term a,
		slTypeBindsOfMetaObject :: [(String, MetaObject a)],
		slTermBindsOfMetaObject :: [(String, MetaObject a)]
	}
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		jsExprLiteralOfMetaObject :: JS.Expression JS.SourcePos,
		slEquivOfMetaObject :: MetaObject a,
		jsTypeOfMetaObject :: MetaObject a,
		jsExprBindsOfMetaObject :: [(String, MetaObject a)]
	}
	| MOJSExprGlobal {
		tagOfMetaObject :: a,
		uniqueIdOfMetaObject :: JSGlobalUniqueId,
		contentOfMetaObject :: MetaObject a,
		slEquivOfMetaObject :: MetaObject a,
		jsTypeOfMetaObject :: MetaObject a
	}
	deriving Show

newtype JSGlobalUniqueId = JSGlobalUniqueId String deriving (Eq, Ord, Show)

-- `Directive` represents a top-level translation language directive.

data Directive a
	= DLet {
		tagOfDirective :: a,
		nameOfDirective :: String,
		paramsOfDirective :: [(String, MetaType a)],
		typeOfDirective :: Maybe (MetaType a),
		valueOfDirective :: MetaObject a
	}
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDirective :: String,
		paramsOfDirective :: [(String, MetaType a)],
		slEquivOfDirective :: MetaObject a
	}
	| DEmit {
		tagOfDirective :: a,
		codeOfDirective :: [JS.Statement JS.SourcePos],
		jsExprBindsOfDirective :: [(String, MetaObject a)]
	}
	deriving Show

