module Metacompiler.Runtime.FreeNames where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Metacompiler.Runtime.Traverse
import Metacompiler.Runtime.Types

data Names = Names {
	metaObjectsInNames :: S.Set NameOfMetaObject,
	slTypesInNames :: S.Set NameOfSLType,
	slTermsInNames :: S.Set NameOfSLTerm
	}

instance Monoid Names where
	mempty = Names S.empty S.empty S.empty
	(Names a1 b1 c1) `mappend` (Names a2 b2 c2) =
		Names (a1 `S.union` a2) (b1 `S.union` b2) (c1 `S.union` c2)

data NamesAndTypes = NamesAndTypes {
	metaObjectsInNamesAndTypes :: M.Map NameOfMetaObject MetaType,
	slTypesInNamesAndTypes :: M.Map NameOfSLType SLKind,
	slTermsInNamesAndTypes :: M.Map NameOfSLTerm MetaObject
	}

instance Monoid NamesAndTypes where
	mempty = NamesAndTypes M.empty M.empty M.empty
	(NamesAndTypes a1 b1 c1) `mappend` (NamesAndTypes a2 b2 c2) =
		NamesAndTypes (a1 `M.union` a2) (b1 `M.union` b2) (c1 `M.union` c2)

namesAndTypesToNames :: NamesAndTypes -> Names
namesAndTypesToNames (NamesAndTypes a b c) =
	Names (S.fromList (M.keys a)) (S.fromList (M.keys b)) (S.fromList (M.keys c))

-- `freeNamesAndTypesInMetaType` and `freeNamesAndTypesInMetaObject` find all the unbound variables in the given term
-- and return their names and types.

freeNamesAndTypesInMetaType :: MetaType -> NamesAndTypes
freeNamesAndTypesInMetaType (MTFun (paramName, paramType) resultType) = let
	paramNames = freeNamesAndTypesInMetaType paramType
	resultNames = freeNamesAndTypesInMetaType resultType
	resultNames' = resultNames { metaObjectsInNamesAndTypes = M.delete paramName (metaObjectsInNamesAndTypes resultNames) }
	in paramNames `mappend` resultNames
freeNamesAndTypesInMetaType other =
	getConst (traverseMetaType freeNamesAndTypesVisitor other)

freeNamesAndTypesInMetaObject :: MetaObject -> NamesAndTypes
freeNamesAndTypesInMetaObject (MOAbs (paramName, paramType) body) = let
	paramNames = freeNamesAndTypesInMetaType paramType
	bodyNames = freeNamesAndTypesInMetaObject body
	bodyNames' = bodyNames { metaObjectsInNamesAndTypes = M.delete paramName (metaObjectsInNamesAndTypes bodyNames) }
	in paramNames `mappend` bodyNames'
freeNamesAndTypesInMetaObject (MOName n type_) =
	mempty { metaObjectsInNamesAndTypes = M.singleton n type_ } `mappend` freeNamesAndTypesInMetaType type_
freeNamesAndTypesInMetaObject (MOSLTypeName n kind) =
	mempty { slTypesInNamesAndTypes = M.singleton n kind }
freeNamesAndTypesInMetaObject (MOSLTermName n type_) =
	mempty { slTermsInNamesAndTypes = M.singleton n type_ }
	`mappend` freeNamesAndTypesInMetaObject type_
freeNamesAndTypesInMetaObject (MOSLTermAbs (paramName, paramType) body) = let
	paramNames = freeNamesAndTypesInMetaObject paramType
	bodyNames = freeNamesAndTypesInMetaObject body
	bodyNames' = bodyNames { slTermsInNamesAndTypes = M.delete paramName (slTermsInNamesAndTypes bodyNames) }
	in paramNames `mappend` bodyNames'
freeNamesAndTypesInMetaObject (MOSLTermCase subject clauses) =
	freeNamesAndTypesInMetaObject subject
	`mappend` mconcat [let
		typeParamNames = mconcat (map freeNamesAndTypesInMetaObject typeParams)
		bodyNames = freeNamesAndTypesInMetaObject body
		bodyNames' = bodyNames { slTermsInNamesAndTypes = foldr M.delete (slTermsInNamesAndTypes bodyNames) fieldNames }
		in typeParamNames `mappend` bodyNames'
		| (_, typeParams, fieldNames, body) <- clauses]
freeNamesAndTypesInMetaObject (MOJSExprLiteral equiv type_ expr bindings) =
	freeNamesAndTypesInMetaObject equiv
	`mappend` freeNamesAndTypesInMetaObject type_
	`mappend` mconcat (map freeNamesAndTypesInBinding (M.elems bindings))
	where
		freeNamesAndTypesInBinding :: JSExprBinding -> NamesAndTypes
		freeNamesAndTypesInBinding (JSExprBinding params value) =
			mconcat [
				freeNamesAndTypesInMetaObject typeOfSL `mappend` freeNamesAndTypesInMetaObject typeOfJS
				| JSExprBindingParam nameOfSL typeOfSL nameOfJS typeOfJS <- params]
			`mappend` (let
				names = freeNamesAndTypesInMetaObject value
				metaObjectNames = metaObjectsInNamesAndTypes names
				metaObjectNames' = foldr M.delete metaObjectNames (concat [[n1, n2] | JSExprBindingParam n1 _ n2 _ <- params])
				in names { metaObjectsInNamesAndTypes = metaObjectNames' }
				)
freeNamesAndTypesInMetaObject other =
	getConst (traverseMetaObject freeNamesAndTypesVisitor other)

freeNamesAndTypesVisitor :: Visitor (Const NamesAndTypes)
freeNamesAndTypesVisitor = Visitor {
	visitMetaType = Const . freeNamesAndTypesInMetaType,
	visitMetaObject = Const . freeNamesAndTypesInMetaObject
	}

-- `globalNamesInMetaType` and `globalNamesInMetaObject` return the names of all the global objects that the given type
-- or term refers to.

globalNamesInMetaType :: MetaType -> Names
globalNamesInMetaType other =
	getConst (traverseMetaType globalNamesVisitor other)

globalNamesInMetaObject :: MetaObject -> Names
globalNamesInMetaObject (MOSLTypeDefn defn) =
	mempty { slTypesInNames = S.singleton (nameOfSLDataDefn defn) }
globalNamesInMetaObject obj@(MOSLTermDefn defn _) =
	mempty { slTermsInNames = S.singleton (nameOfSLTermDefn defn) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOSLTermCase _ clauses) =
	mempty { slTermsInNames = S.fromList [nameOfSLCtorDefn ctor | (ctor, _, _, _) <- clauses] }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOSLTermData ctor _ _) =
	mempty { slTermsInNames = S.singleton (nameOfSLCtorDefn ctor) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject obj@(MOJSExprTypeDefn defn _) =
	mempty { metaObjectsInNames = S.singleton (nameOfJSExprTypeDefn defn) }
	`mappend` globalNamesInMetaObject obj
globalNamesInMetaObject other =
	getConst (traverseMetaObject globalNamesVisitor other)

globalNamesVisitor :: Visitor (Const Names)
globalNamesVisitor = Visitor {
	visitMetaType = Const . globalNamesInMetaType,
	visitMetaObject = Const . globalNamesInMetaObject
	}

-- Minor variations on the above functions

freeNamesInMetaObject :: MetaObject -> Names
freeNamesInMetaObject = namesAndTypesToNames . freeNamesAndTypesInMetaObject

freeNamesInMetaType :: MetaType -> Names
freeNamesInMetaType = namesAndTypesToNames . freeNamesAndTypesInMetaType

freeAndGlobalNamesInMetaObject :: MetaObject -> Names
freeAndGlobalNamesInMetaObject o =
	freeNamesInMetaObject o `mappend` globalNamesInMetaObject o

freeAndGlobalNamesInMetaType :: MetaType -> Names
freeAndGlobalNamesInMetaType t =
	freeNamesInMetaType t `mappend` globalNamesInMetaType t

