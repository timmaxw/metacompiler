module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Data.Set as S
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
	| MTJSExprType {
		tagOfMetaType :: a
		slTypeOfMTJSExprType :: MetaObject a
	}
	| MTJSExpr {
		tagOfMetaType :: a,
		jsTypeOfMTJSExpr :: MetaObject a
		slTermOfMTJSExpr :: MetaObject a,
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
	| MOJSExprLiteral {
		tagOfMetaObject :: a,
		slTermOfMOJSExprLiteral :: MetaObject a,
		jsTypeOfMOJSExprLiteral :: MetaObject a,
		codeOfMOJSExprLiteral :: JS.Expression JS.SourcePos,
		bindingsOfMOJSExprLiteral :: [Binding a (JS.Id ())]
	}
	| MOJSExprGlobal {
		tagOfMetaObject :: a,
		slTermOfMOJSExprGlobal :: MetaObject a,
		jsTypeOfMOJSExprGlobal :: MetaObject a,
		uniqueIdOfMOJSExprGlobal :: JSGlobalUniqueId,
		contentOfMOJSExprGlobal :: MetaObject a
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
{-
	| DJSExprType {
		tagOfDirective :: a,
		nameOfDJSExprType :: Name,
		paramsOfDJSExprType :: [(Name, MetaType a)],
		slEquivOfDJSExprType :: MetaObject a
	}
	| DJSEmit {
		tagOfDirective :: a,
		codeOfDJSEmit :: [JS.Statement JS.SourcePos],
		bindJSExprsOfDJSEmit :: [Binding a (JS.Id ())]
	}
-}
	deriving Show

freeNamesInMetaType :: MetaType a -> S.Set Name
freeNamesInMetaType (MTFun _ params result) = f params
	where
		f :: [(Name, MetaType a)] -> S.Set Name
		f [] = freeNamesInMetaType result
		f ((paramName, paramType):rest) =
			freeNamesInMetaType paramType `S.union` S.delete paramName (f rest)
freeNamesInMetaType (MTSLType _ _) = S.empty
freeNamesInMetaType (MTSLTerm _ type_) = freeNamesInMetaObject type_

freeNamesInMetaObject :: MetaObject a -> S.Set Name
freeNamesInMetaObject (MOApp _ fun arg) =
	freeNamesInMetaObject fun `S.union` freeNamesInMetaObject arg
freeNamesInMetaObject (MOAbs _ params result) = f params
	where
		f :: [(Name, MetaType a)] -> S.Set Name
		f [] = freeNamesInMetaObject result
		f ((paramName, paramType):rest) =
			freeNamesInMetaType paramType `S.union` S.delete paramName (f rest)
freeNamesInMetaObject (MOName _ name) = S.singleton name
freeNamesInMetaObject (MOSLTypeLiteral _ _ bindings) =
	S.unions (map freeNamesInBinding bindings)
freeNamesInMetaObject (MOSLTermLiteral _ _ typeBindings termBindings) =
	S.unions (map freeNamesInBinding typeBindings)
	`S.union` S.unions (map freeNamesInBinding termBindings)

freeNamesInBinding :: Binding a n -> S.Set Name
freeNamesInBinding (Binding _ _ params value) = f params
	where
		f [] = freeNamesInMetaObject value
		f (BindingParam []:params) = f params
		f (BindingParam ((paramName, paramType):paramParts):params) =
			freeNamesInMetaType paramType
			`S.union` S.delete paramName (f (BindingParam paramParts:params))

