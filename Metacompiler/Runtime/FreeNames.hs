module Metacompiler.Runtime.FreeNames where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Metacompiler.Runtime.Traverse
import Metacompiler.Runtime.Types

-- `freeNamesInMetaType` and `freeNamesInMetaObject` return sets of all unbound TL and SL variables that appear in the
-- given meta-type or meta-object.

data FreeNames = FreeNames {
	namesInFreeNames :: S.Set Name,
	namesOfSLTypesInFreeNames :: S.Set NameOfSLType,
	namesOfSLTermsInFreeNames :: S.Set NameOfSLTerm
	}

instance Monoid FreeNames where
	mempty = FreeNames S.empty S.empty S.empty
	mappend (FreeNames a1 b1 c1) (FreeNames a2 b2 c2) =
		FreeNames (S.union a1 a2) (S.union b1 b2) (S.union c1 c2)

freeNamesInMetaType :: MetaType -> FreeNames
freeNamesInMetaType (MTFun (paramName, paramType) resultType) = let
	paramNames = freeNamesInMetaType paramType
	resultNames = freeNamesInMetaType resultType
	resultNames' = resultNames { namesInFreeNames = S.delete paramName (namesInFreeNames resultNames) }
	in paramNames `mappend` resultNames
freeNamesInMetaType other =
	getConst (traverseMetaType freeNamesVisitor other)

freeNamesInMetaObject :: MetaObject -> FreeNames
freeNamesInMetaObject (MOAbs (paramName, paramType) body) = let
	paramNames = freeNamesInMetaType paramType
	bodyNames = freeNamesInMetaObject body
	bodyNames' = bodyNames { namesInFreeNames = S.delete paramName (namesInFreeNames bodyNames) }
	in paramNames `mappend` bodyNames'
freeNamesInMetaObject (MOName n type_) =
	FreeNames (S.singleton n) S.empty S.empty `mappend` freeNamesInMetaType type_
freeNamesInMetaObject (MOSLTypeName n _) =
	FreeNames S.empty (S.singleton n) S.empty
freeNamesInMetaObject (MOSLTermDefn _ typeParams) =
	mconcat (map freeNamesInMetaObject typeParams)
freeNamesInMetaObject (MOSLTermName n type_) =
	FreeNames S.empty S.empty (S.singleton n)
	`mappend` freeNamesInMetaObject type_
freeNamesInMetaObject (MOSLTermAbs (paramName, paramType) body) = let
	paramNames = freeNamesInMetaObject paramType
	bodyNames = freeNamesInMetaObject body
	bodyNames' = bodyNames { namesOfSLTermsInFreeNames = S.delete paramName (namesOfSLTermsInFreeNames bodyNames) }
	in paramNames `mappend` bodyNames'
freeNamesInMetaObject (MOSLTermCase subject clauses) =
	freeNamesInMetaObject subject
	`mappend` mconcat [let
		typeParamNames = mconcat (map freeNamesInMetaObject typeParams)
		bodyNames = freeNamesInMetaObject body
		bodyNames' = bodyNames { namesOfSLTermsInFreeNames = foldr S.delete (namesOfSLTermsInFreeNames bodyNames) fieldNames }
		in typeParamNames `mappend` bodyNames'
		| (_, typeParams, fieldNames, body) <- clauses]
freeNamesInMetaObject (MOJSExprLiteral equiv type_ expr bindings) =
	freeNamesInMetaObject equiv
	`mappend` freeNamesInMetaObject type_
	`mappend` mconcat (map freeNamesInBinding (M.elems bindings))
	where
		freeNamesInBinding :: JSExprBinding -> FreeNames
		freeNamesInBinding (JSExprBinding params value) =
			mconcat [
				freeNamesInMetaObject typeOfSL `mappend` freeNamesInMetaObject typeOfJS
				| JSExprBindingParam nameOfSL typeOfSL nameOfJS typeOfJS <- params]
			`mappend` (let
				freeNamesInValue = freeNamesInMetaObject value
				names = namesInFreeNames freeNamesInValue
				names' = (S.\\) names (S.unions [S.fromList [n1, n2] | JSExprBindingParam n1 _ n2 _ <- params])
				in freeNamesInValue { namesInFreeNames = names' }
				)
freeNamesInMetaObject other =
	getConst (traverseMetaObject freeNamesVisitor other)

freeNamesVisitor :: Visitor (Const FreeNames)
freeNamesVisitor = Visitor {
	visitMetaType = Const . freeNamesInMetaType,
	visitMetaObject = Const . freeNamesInMetaObject
	}

-- `globalNamesInMetaType` and `globalNamesInMetaObject` return the names of all the global objects that the given type
-- or term refers to.

globalNamesInMetaType :: MetaType -> FreeNames
globalNamesInMetaType other =
	getConst (traverseMetaType freeNamesVisitor other)

globalNamesInMetaObject :: MetaObject -> FreeNames
globalNamesInMetaObject (MOSLTypeDefn defn) =
	mempty { namesOfSLTypesInFreeNames = S.singleton (nameOfSLDataDefn defn) }
globalNamesInMetaObject obj@(MOSLTermDefn defn) =
	mempty { namesOfSLTermsInFreeNames = S.singleton (nameOfSLTermDefn defn) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOSLTermCase _ clauses) =
	mempty { namesOfSLTermsInFreeNames = S.fromList [NameOfSLTerm (unNameOfSLCtor (nameOfSLCtorDefn ctor)) | (ctor, _, _, _) <- clauses] }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOSLTermData ctor _ _) =
	mempty { namesOfSLTermsInFreeNames = S.singleton (NameOfSLTerm (unNameOfSLCtor (nameOfSLCtorDefn ctor))) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOJSExprTypeDefn defn _) =
	mempty { namesInFreeNames = S.singleton (nameOfJSExprTypeDefn defn) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject other =
	getConst (traverseMetaObject globalNamesVisitor other)

globalNamesVisitor :: Visitor (Const FreeNames)
globalNamesVisitor = Visitor {
	visitMetaType = Const . globalNamesInMetaType,
	visitMetaObject = Const . globalNamesInMetaObject
	}

-- `freeAndGlobalNamesInMetaType` and `freeAndGlobalNamesInMetaObject` return both free and global names.

freeAndGlobalNamesInMetaObject :: MetaObject -> FreeNames
freeAndGlobalNamesInMetaObject o =
	freeNamesInMetaObject o `mappend` globalNamesInMetaObject o

freeAndGlobalNamesInMetaType :: MetaType -> FreeNames
freeAndGlobalNamesInMetaType t =
	freeNamesInMetaType t `mappend` globalNamesInMetaType t

