module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Metacompiler.SLSyntax as SLS

newtype Name = Name { unName :: String } deriving (Eq, Ord)

-- `MetaType` represents a translation-language meta-type. It is more of a
-- syntactic representation than a semantic one; for example, it doesn't
-- represent `fun ... -> ...` blocks with multiple arguments down into multiple
-- single-argument blocks. It is parameterized on a "tag" type, which will
-- typically be `Range`.

data MetaType a
	= MTFun {
		tagOfMetaType :: a,
		paramsOfMTFun :: [(Name, MetaType a)],
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
		paramsOfMOAbs :: [(Name, MetaType a)],
		resultOfMOAbs :: MetaObject a
	}
	| MOName {
		tagOfMetaObject :: a,
		varOfMOName :: Name
	}
	| MOSLTypeLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTypeLiteral :: SLS.Type a,
		slTypeBindsOfMOSLTypeLiteral :: [(SLS.Name, [Name], MetaObject a)]
	}
	| MOSLTermLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTermLiteral :: SLS.Term a,
		slTypeBindsOfMOSLTermLiteral :: [(SLS.Name, [Name], MetaObject a)],
		slTermBindsOfMOSLTermLiteral :: [(SLS.Name, [Name], MetaObject a)]
	}
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		codeOfMOJSExprLiteral :: JS.Expression JS.SourcePos,
		jsExprBindsOfMOJSExprLiteral :: [(JS.Id(), [(Name, MetaObject a, Name)], MetaObject a)]
	}
	| MOJSStatementLiteral {
		tagOfMetaObject :: a,
		codeOfMOJSStatementLiteral :: [JS.Statement JS.SourcePos],
		jsExprNamesOfMOJSStatementLiteral :: [(JS.Id (), Name)],
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
		paramsOfDLet :: [(Name, MetaType a)],
		typeOfDLet :: Maybe (MetaType a),
		valueOfDLet :: MetaObject a
	}
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDJSExprType :: Name,
		paramsOfDJSExprType :: [(Name, MetaType a)],
		slEquivOfDJSExprType :: MetaObject a
	}
	| DEmit {
		tagOfDirective :: a,
		contentOfDEmit :: MetaObject a
	}
	deriving Show

