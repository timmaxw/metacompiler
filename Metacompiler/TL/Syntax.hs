module Metacompiler.TL.Syntax where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.JS as JS
import qualified Metacompiler.SL.Syntax as SL

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
		slKindOfMTSLType :: SL.Kind a
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
		codeOfMOSLTypeLiteral :: SL.Type a,
		typeBindingsOfMOSLTypeLiteral :: [Binding a SL.NameOfType]
	}
	| MOSLTermLiteral {
		tagOfMetaObject :: a,
		codeOfMOSLTermLiteral :: SL.Term a,
		typeBindingsOfMOSLTermLiteral :: [Binding a SL.NameOfType],
		termBindingsOfMOSLTermLiteral :: [Binding a SL.NameOfTerm]
	}
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		slTermOfMOJSExprLiteral :: MetaObject a,
		jsTypeOfMOJSExprLiteral :: MetaObject a,
		codeOfMOJSExprLiteral :: JS.Expression JS.SourcePos,
		bindingsOfMOJSExprLiteral :: [Binding a (JS.Id ())]
	}
	| MOJSExprLoopBreak {
		tagOfMetaObject :: a,
		slTermOfMOJSExprLoopBreak :: MetaObject a,
		jsTypeOfMOJSExprLoopBreak :: MetaObject a,
		contentOfMOJSExprLoopBreak :: MetaObject a
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
		contentOfDSLCode :: [SL.Dir a]
	}
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDJSExprType :: Name,
		paramsOfDJSExprType :: [(Name, MetaType a)],
		slEquivOfDJSExprType :: MetaObject a
	}
	| DJSEmit {
		tagOfDirective :: a,
		codeOfDJSEmit :: [JS.Statement JS.SourcePos],
		bindingsOfDJSEmit :: [Binding a (JS.Id ())]
	}
	deriving Show

