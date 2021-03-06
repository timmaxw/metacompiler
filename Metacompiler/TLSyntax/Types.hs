module Metacompiler.TLSyntax.Types where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.JS.JS as JS
import qualified Metacompiler.SLSyntax.Types as SLS

newtype Name = Name { unName :: String } deriving (Eq, Ord, Show)

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
		tagOfMetaType :: a,
		slKindOfMTSLType :: SLS.Kind a
	}
	| MTSLTerm {
		tagOfMetaType :: a,
		slTypeOfMTSLTerm :: MetaObject a
	}
	| MTJSExprType {
		tagOfMetaType :: a,
		slTypeOfMTJSExprType :: MetaObject a
	}
	| MTJSExpr {
		tagOfMetaType :: a,
		jsTypeOfMTJSExpr :: MetaObject a,
		slTermOfMTJSExpr :: MetaObject a
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
		typeBindingsOfMOSLTypeLiteral :: [Binding a SLS.NameOfType]
	}
	| MOSLTermLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTermLiteral :: SLS.Term a,
		typeBindingsOfMOSLTermLiteral :: [Binding a SLS.NameOfType],
		termBindingsOfMOSLTermLiteral :: [Binding a SLS.NameOfTerm]
	}
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		slTermOfMOJSExprLiteral :: MetaObject a,
		jsTypeOfMOJSExprLiteral :: MetaObject a,
		codeOfMOJSExprLiteral :: JS.Expression JS.SourcePos,
		bindingsOfMOJSExprLiteral :: [Binding a (JS.Id ())]
	}
	| MOJSExprConvertEquiv {
		tagOfMetaObject :: a,
		inEquivOfMOJSExprConvertEquiv :: MetaObject a,
		outEquivOfMOJSExprConvertEquiv :: MetaObject a,
		contentOfMOJSExprConvertEquiv :: MetaObject a
		}
	deriving Show

data Binding a name = Binding {
	tagOfBinding :: a,
	nameOfBinding :: name,
	paramsOfBinding :: [BindingParam a],
	valueOfBinding :: MetaObject a
	} deriving Show
 
data BindingParam a = BindingParam {
	tagOfBindingParam :: a,
	partsOfBindingParam :: [(Name, MetaType a)]
	} deriving Show

-- `Directive` represents a top-level translation language directive.

data Directive a
	= DLet {
		tagOfDirective :: a,
		nameOfDLet :: Name,
		paramsOfDLet :: [(Name, MetaType a)],
		typeOfDLet :: Maybe (MetaType a),
		valueOfDLet :: MetaObject a
	}
	| DSLCode {
		tagOfDirective :: a,
		contentOfDSLCode :: [SLS.Dir a]
	}
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDJSExprType :: Name,
		paramsOfDJSExprType :: [(Name, MetaType a)],
		slEquivOfDJSExprType :: MetaObject a
	}
	| DJSExprGlobal {
		tagOfDirective :: a,
		nameOfDJSExprGlobal :: Name,
		paramsOfDJSExprGlobal :: [(Name, MetaType a)],
		runtimeArgsOfDJSExprGlobal :: [Name],
		typeOfDJSExprGlobal :: MetaObject a,
		specOfDJSExprGlobal :: MetaObject a,
		bodyOfDJSExprGlobal :: MetaObject a
		}
	| DJSExprUse {
		tagOfDirective :: a,
		paramsOfDJSExprUse :: [(Name, MetaType a)],
		bodyOfDJSExprUse :: MetaObject a
		}
	| DJSExprInfer {
		tagOfDirective :: a,
		nameOfDJSExprInfer :: Name,
		typeOfDJSExprInfer :: MetaObject a,
		specOfDJSExprInfer :: MetaObject a
		}
	| DJSEmit {
		tagOfDirective :: a,
		codeOfDJSEmit :: [JS.Statement JS.SourcePos],
		bindingsOfDJSEmit :: [Binding a (JS.Id ())]
	}
	deriving Show

