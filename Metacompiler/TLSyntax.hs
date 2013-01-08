module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Metacompiler.SLSyntax as SLS

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
{-
	| MTJSEquivExprType {
		tagOfMetaType :: a
		slTypeOfMTJSEquivExprType :: MetaObject a
	}
	| MTJSEquivExpr {
		tagOfMetaType :: a,
		slTermOfMTJSEquivExpr :: MetaObject a,
		jsEquivExprTypeOfMTJSEquivExpr :: MetaObject a
	}
-}
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
{-
	| MOJSEquivExprLiteral {
		tagOfMetaObject :: a,
		slTermOfMOJSEquivExprLiteral :: MetaObject a,
		jsTypeOfMOJSEquivExprLiteral :: MetaObject a,
		codeOfMOJSEquivExprLiteral :: JS.Expression JS.SourcePos,
		bindingsOfMOJSExprLiteral :: [Binding a (JS.Id ())]
	}
	| MOJSEquivExprGlobal {
		tagOfMetaObject :: a,
		slTermOfMOJSEquivExprGlobal :: MetaObject a,
		jsTypeOfMOJSEquivExprGlobal :: MetaObject a,
		uniqueIdOfMOJSEquivExprGlobal :: JSGlobalUniqueId,
		contentOfMOJSEquivExprGlobal :: MetaObject a
	}
-}
	deriving Show

{-
newtype JSGlobalUniqueId = JSGlobalUniqueId String deriving (Eq, Ord, Show)
-}

data Binding a name = Binding {
	tagOfBinding :: a,
	nameOfBinding :: name,
	paramsOfBinding :: [BindingParam a],
	valueOfBinding :: MetaObject a
	} deriving Show
 
data BindingParam a = BindingParam [(Name, MetaType a)] deriving Show

-- `Directive` represents a top-level translation language directive.

{-
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
		codeOfDEmit :: [JS.Statement JS.SourcePos],
		bindJSEquivExprsOfDEmit :: [Binding a (JS.Id ())]
	}
	deriving Show
-}

