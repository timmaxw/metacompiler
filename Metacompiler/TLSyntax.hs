module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Metacompiler.SLSyntax as SLS

newtype Var = Var { unVar :: String } deriving (Eq, Ord)

-- `MetaType` represents a translation-language meta-type. It is more of a
-- syntactic representation than a semantic one; for example, it doesn't
-- represent `fun ... -> ...` blocks with multiple arguments down into multiple
-- single-argument blocks. It is parameterized on a "tag" type, which will
-- typically be `Range`.

data MetaType a
	= MTFun {
		tagOfMetaType :: a,
		paramsOfMTFun :: [(Var, MetaType a)],
		resultOfMTFun :: MetaType a
	}
	| MTSLType {
		tagOfMetaType :: a
	}
	| MTSLTerm {
		tagOfMetaType :: a,
		slTypeOfMTSLTerm :: MetaObject a
	}
	| MTJSExpr {
		tagOfMetaType :: a
	}
	| MTJSStatement {
		tagOfMetaType :: a
	}
	| MTJSEquivExprType {
		tagOfMetaType :: a
		slTypeOfMTJSEquivExprType :: MetaObject a
	}
	| MTJSEquivExpr {
		tagOfMetaType :: a,
		slTermOfMTJSEquivExpr :: MetaObject a,
		jsEquivExprTypeOfMTJSEquivExpr :: MetaObject a
	}
	deriving Show

-- `MetaObject` represents a translation-language meta-object.

data MetaObject a
	= MOApp {
		tagOfMetaObject :: a,
		funOfMOApp :: MetaObject a,
		argOfMOApp :: MetaObject a
	}
	| MOAbs {
		tagOfMetaObject :: a,
		paramsOfMOAbs :: [(Var, MetaType a)],
		resultOfMOAbs :: MetaObject a
	}
	| MOVar {
		tagOfMetaObject :: a,
		varOfMOVar :: String
	}
	| MOSLTypeLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTypeLiteral :: SLS.Type a,
		slTypeBindsOfMOSLTypeLiteral :: [(SLS.Var, MetaObject a)]
	}
	| MOSLTermLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTermLiteral :: SLS.Term a,
		slTypeBindsOfMOSLTermLiteral :: [(SLS.Var, MetaObject a)],
		slTermVarsOfMOSLTermLiteral :: [(SLS.Var, Var)],
		slTermBindsOfMOSLTermLiteral :: [(SLS.Var, MetaObject a)]
	}
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		codeOfMOJSExprLiteral :: JS.Expression JS.SourcePos,
		jsExprVarsOfMOJSExprLiteral :: [(JS.Id (), Var)],
		jsExprBindsOfMOJSExprLiteral :: [(JS.Id(), MetaObject a)]
	}
	| MOJSStatementLiteral {
		tagOfMetaObject :: a,
		codeOfMOJSStatementLiteral :: [JS.Statement JS.SourcePos],
		jsExprVarsOfMOJSStatementLiteral :: [(JS.Id (), Var)],
		jsExprBindsOfMOJSStatementLiteral :: [(JS.Id(), MetaObject a)]
	}
	| MOJSEquivExprWrap {
		tagOfMetaObject :: a,
		slTermOfMOJSEquivExprLiteral :: MetaObject a,
		jsTypeOfMOJSEquivExprLiteral :: MetaObject a,
		jsExprOfMOJSEquivExprLiteral :: MetaObject a
	}
	| MOJSEquivExprGlobal {
		tagOfMetaObject :: a,
		slTermOfMOJSEquivExprGlobal :: MetaObject a,
		jsTypeOfMOJSEquivExprGlobal :: MetaObject a,
		uniqueIdOfMOJSEquivExprGlobal :: JSGlobalUniqueId,
		contentOfMOJSEquivExprGlobal :: MetaObject a
	}
	| MOJSEquivExprUnwrap {
		tagOfMetaObject :: a,
		contentOfMOJSEquivExprUnwrap :: MetaObject a
	}	
	deriving Show

newtype JSGlobalUniqueId = JSGlobalUniqueId String deriving (Eq, Ord, Show)

-- `Directive` represents a top-level translation language directive.

data Directive a
	= DLet {
		tagOfDirective :: a,
		nameOfDLet :: String,
		paramsOfDLet :: [(String, MetaType a)],
		typeOfDLet :: Maybe (MetaType a),
		valueOfDLet :: MetaObject a
	}
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDJSExprType :: String,
		paramsOfDJSExprType :: [(String, MetaType a)],
		slEquivOfDJSExprType :: MetaObject a
	}
	| DEmit {
		tagOfDirective :: a,
		contentOfDEmit :: MetaObject a
	}
	deriving Show

