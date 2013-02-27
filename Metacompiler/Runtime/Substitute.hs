module Metacompiler.Runtime.Substitute where

import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Metacompiler.JS as JS
import Metacompiler.Runtime.FreeNames
import Metacompiler.Runtime.Traverse
import Metacompiler.Runtime.Types

-- `substituteMetaType` and `substituteMetaObject` traverse the given meta-type or meta-object; whenever they encounter
-- a reference to a free variable that appears in the given map, they replace it with its value from the map. They
-- operate on both TL names and SL names simultaneously. They correctly implement name shadowing and avoid capturing
-- variables. For example, replacing `a` with `b` in `fun (a :: ...) -> a` will leave it unchanged, and replacing `a`
-- with `b` in `fun (b :: ...) -> a` will produce `fun (b' :: ...) -> b`.

data Substitutions = Substitutions {
	metaObjectSubstitutions :: M.Map NameOfMetaObject MetaObject,
	slTypeSubstitutions :: M.Map NameOfSLType MetaObject,
	slTermSubstitutions :: M.Map NameOfSLTerm MetaObject
	}

substituteMetaType :: Substitutions -> MetaType -> MetaType
substituteMetaType subs (MTFun (paramName, paramType) returnType) = let
	paramType' = substituteMetaType subs paramType
	(subs', [paramName']) = prepareForBindingMetaObjects (freeNamesInMetaType returnType) [paramType'] (subs, [paramName])
	returnType' = substituteMetaType subs' returnType
	in MTFun (paramName', paramType') returnType'
substituteMetaType subs other = runIdentity (traverseMetaType (makeSubstitutionVisitor subs) other)

substituteMetaObject :: Substitutions -> MetaObject -> MetaObject
substituteMetaObject subs (MOAbs (paramName, paramType) body) = let
	paramType' = substituteMetaType subs paramType
	(subs', [paramName']) = prepareForBindingMetaObjects (freeNamesInMetaObject body) [paramType'] (subs, [paramName])
	body' = substituteMetaObject subs' body
	in MOAbs (paramName', paramType') body'
substituteMetaObject subs (MOName name type_) = case M.lookup name (metaObjectSubstitutions subs) of
	Just value -> value
	Nothing -> MOName name (substituteMetaType subs type_)
substituteMetaObject subs (MOSLTypeDefn defn) = case M.lookup (nameOfSLDataDefn defn) (slTypeSubstitutions subs) of
	Just _ -> error "trying to substitute the name of a global variable"
	Nothing -> MOSLTypeDefn defn
substituteMetaObject subs (MOSLTypeName name kind) = case M.lookup name (slTypeSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTypeName name kind
substituteMetaObject subs (MOSLTermDefn defn typeParams) = case M.lookup (nameOfSLTermDefn defn) (slTermSubstitutions subs) of
	Just _ -> error "trying to substitute the name of a global variable"
	Nothing -> MOSLTermDefn defn (map (substituteMetaObject subs) typeParams)
substituteMetaObject subs (MOSLTermName name type_) = case M.lookup name (slTermSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTermName name (substituteMetaObject subs type_)
substituteMetaObject subs (MOSLTermAbs (paramName, paramType) body) = let
	paramType' = substituteMetaObject subs paramType
	(subs', [paramName']) = prepareForBindingSLTerms (freeNamesInMetaObject body) [paramType'] (subs, [paramName])
	body' = substituteMetaObject subs' body
	in MOSLTermAbs (paramName', paramType') body'
substituteMetaObject subs (MOSLTermCase subject clauses) = let
	subject' = substituteMetaObject subs subject
	clauses' = [let
		namesInBody = freeNamesInMetaObject body
		typeParams' = map (substituteMetaObject subs) typeParams
		fieldTypes = map ($ typeParams') (fieldTypesOfSLCtorDefn ctor)
		(subs', fieldNames') = prepareForBindingSLTerms namesInBody fieldTypes (subs, fieldNames)
		body' = substituteMetaObject subs' body
		in (ctor, typeParams', fieldNames', body')
		| (ctor, typeParams, fieldNames, body) <- clauses]
	in MOSLTermCase subject' clauses'
substituteMetaObject subs (MOJSExprTypeDefn defn params) = case M.lookup (nameOfJSExprTypeDefn defn) (metaObjectSubstitutions subs) of
	Just _ -> error "trying to substitute the name of a global variable"
	Nothing -> MOJSExprTypeDefn defn (map (substituteMetaObject subs) params)
substituteMetaObject subs (MOJSExprLiteral equiv type_ expr bindings) = let
	equiv' = substituteMetaObject subs equiv
	type_' = substituteMetaObject subs type_
	bindings' = M.map (\(JSExprBinding params value) -> let
		namesInValue = freeNamesInMetaObject value
		(slParamNames, slParamTypes, jsParamNames, jsParamTypes) = unzip4 [
			(n1, t1, n2, t2)
			| JSExprBindingParam n1 t1 n2 t2 <- params]
		slParamTypes' = map (substituteMetaObject subs) slParamTypes
		fullSLParamTypes = [MTSLTerm t | t <- slParamTypes']
		(subs', slParamNames') = prepareForBindingMetaObjects namesInValue fullSLParamTypes (subs, slParamNames)
		jsParamTypes' = map (substituteMetaObject subs') jsParamTypes
		fullJSParamTypes = [MTJSExpr t2 (MOName n1 t1) | (t2, n1, t1) <- zip3 jsParamTypes' slParamNames' fullSLParamTypes]
		(subs'', jsParamNames') = prepareForBindingMetaObjects namesInValue fullJSParamTypes (subs, jsParamNames)
		params' = zipWith4 JSExprBindingParam slParamNames' slParamTypes' jsParamNames' jsParamTypes'
		value' = substituteMetaObject subs'' value
		in JSExprBinding params' value'
		) bindings
	in MOJSExprLiteral equiv' type_' expr bindings'
substituteMetaObject subs other = runIdentity (traverseMetaObject (makeSubstitutionVisitor subs) other)

makeSubstitutionVisitor :: Substitutions -> Visitor Identity
makeSubstitutionVisitor subs = Visitor {
	visitMetaType = Identity . substituteMetaType subs,
	visitMetaObject = Identity . substituteMetaObject subs
	}

-- `prepareForBindingMetaObjects` is a helper function used when performing substitutions on a meta-type or meta-object
-- which introduces one or more new variables into scope. `names` are the new variables being introduced into scope,
-- and `nameTypes` are their meta-types. `namesWithin` are the variables that are free in the part of the term
-- where `names` are in scope. `subs` are the substitutions to be performed. The return value is new values for `subs`
-- and `names`. It performs two jobs:
--  1. It correctly implements name shadowing by removing `names` from `subs` if they appear there
--  2. It implements capture-avoiding substitution by checking whether any of the values of the `subs` map contain
--     names from `names`, and changing that part of `names` to an unused variable if so. If it is necessary to change
--     `names`, it will also make a new entry in `subs` to perform the change.

prepareForBindingMetaObjects :: Names
                             -> [MetaType]
                             -> (Substitutions, [NameOfMetaObject])
                             -> (Substitutions, [NameOfMetaObject])
prepareForBindingMetaObjects namesWithin nameTypes (subs, names) = let
	subs' = subs { metaObjectSubstitutions = foldr M.delete (metaObjectSubstitutions subs) names }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> metaObjectsInNames (freeAndGlobalNamesInMetaObject mo))
					(M.lookup name (metaObjectSubstitutions subs'))
				| name <- S.toList (metaObjectsInNames namesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> metaObjectsInNames (freeAndGlobalNamesInMetaObject mo))
					(M.lookup name (slTermSubstitutions subs'))
				| name <- S.toList (slTermsInNames namesWithin)]
	processNames :: [NameOfMetaObject] -> [(NameOfMetaObject, MetaType)] -> Substitutions -> (Substitutions, [NameOfMetaObject])
	processNames processed [] innerSubs = (innerSubs, processed)
	processNames processed ((name, nameType):toProcess) innerSubs = let
		forbidden = incomingNames
			`S.union` S.delete name ((S.\\)
				(metaObjectsInNames namesWithin)
				(S.fromList (M.keys (metaObjectSubstitutions innerSubs)))
				)
			`S.union` S.fromList processed
		candidates = [NameOfMetaObject (unNameOfMetaObject name ++ replicate n '\'') | n <- [0..]]
		Just name' = find (`S.notMember` forbidden) candidates
		innerSubs' = if name' == name
			then innerSubs
			else innerSubs { metaObjectSubstitutions = M.insert name (MOName name' nameType) (metaObjectSubstitutions innerSubs) }
		in processNames (processed ++ [name']) toProcess innerSubs'
	in processNames [] (zip names nameTypes) subs'

-- `prepareForBindingSLTerms` is like `prepareForBindingMetaObjects` except that it's for terms which introduce new
-- names into SL's term scope.

prepareForBindingSLTerms :: Names
                         -> [MetaObject]
                         -> (Substitutions, [NameOfSLTerm])
                         -> (Substitutions, [NameOfSLTerm])
prepareForBindingSLTerms namesWithin nameTypes (subs, names) = let
	subs' = subs { slTermSubstitutions = foldr M.delete (slTermSubstitutions subs) names }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> slTermsInNames (freeAndGlobalNamesInMetaObject mo))
					(M.lookup name (metaObjectSubstitutions subs'))
				| name <- S.toList (metaObjectsInNames namesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> slTermsInNames (freeAndGlobalNamesInMetaObject mo))
					(M.lookup name (slTermSubstitutions subs'))
				| name <- S.toList (slTermsInNames namesWithin)]
	processNames :: [NameOfSLTerm] -> [(NameOfSLTerm, MetaObject)] -> Substitutions -> (Substitutions, [NameOfSLTerm])
	processNames processed [] innerSubs = (innerSubs, processed)
	processNames processed ((name, nameType):toProcess) innerSubs = let
		forbidden = incomingNames
			`S.union` S.delete name ((S.\\)
				(slTermsInNames namesWithin)
				(S.fromList (M.keys (slTermSubstitutions innerSubs)))
				)
			`S.union` S.fromList processed
		candidates = [NameOfSLTerm (unNameOfSLTerm name ++ replicate n '\'') | n <- [0..]]
		Just name' = find (`S.notMember` forbidden) candidates
		innerSubs' = if name' == name
			then innerSubs
			else innerSubs { slTermSubstitutions = M.insert name (MOSLTermName name' nameType) (slTermSubstitutions innerSubs) }
		in processNames (processed ++ [name']) toProcess innerSubs'
	in processNames [] (zip names nameTypes) subs'

