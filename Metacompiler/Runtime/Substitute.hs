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
	nameSubstitutions :: M.Map Name MetaObject,
	nameOfSLTypeSubstitutions :: M.Map NameOfSLType MetaObject,
	nameOfSLTermSubstitutions :: M.Map NameOfSLTerm MetaObject,
	nameOfJSExprSubstitutions :: M.Map (JS.Id ()) JSExprSubstitution
	}

data JSExprSubstitution
	= DirectJSExprSubstitution (JS.Expression ())
	| FunctionJSExprSubstitution ([JS.Expression ()] -> JS.Expression ())

substituteMetaType :: Substitutions -> MetaType -> MetaType
substituteMetaType subs (MTFun (paramName, paramType) returnType) = let
	paramType' = substituteMetaType subs paramType
	(subs', [paramName']) = prepareForBindingNames (freeNamesInMetaType returnType) [paramType'] (subs, [paramName])
	returnType' = substituteMetaType subs' returnType
	in MTFun (paramName', paramType') returnType'
substituteMetaType subs other = runIdentity (traverseMetaType (makeSubstitutionVisitor subs) other)

substituteMetaObject :: Substitutions -> MetaObject -> MetaObject
substituteMetaObject subs (MOAbs (paramName, paramType) body) = let
	paramType' = substituteMetaType subs paramType
	(subs', [paramName']) = prepareForBindingNames (freeNamesInMetaObject body) [paramType'] (subs, [paramName])
	body' = substituteMetaObject subs' body
	in MOAbs (paramName', paramType') body'
substituteMetaObject subs (MOName name type_) = case M.lookup name (nameSubstitutions subs) of
	Just value -> value
	Nothing -> MOName name (substituteMetaType subs type_)
substituteMetaObject subs (MOSLTypeName name kind) = case M.lookup name (nameOfSLTypeSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTypeName name kind
substituteMetaObject subs (MOSLTermName name type_) = case M.lookup name (nameOfSLTermSubstitutions subs) of
	Just value -> value
	Nothing -> MOSLTermName name (substituteMetaObject subs type_)
substituteMetaObject subs (MOSLTermAbs (paramName, paramType) body) = let
	paramType' = substituteMetaObject subs paramType
	(subs', [paramName']) = prepareForBindingNamesOfSLTerms (freeNamesInMetaObject body) [paramType'] (subs, [paramName])
	body' = substituteMetaObject subs' body
	in MOSLTermAbs (paramName', paramType') body'
substituteMetaObject subs (MOSLTermCase subject clauses) = let
	subject' = substituteMetaObject subs subject
	clauses' = [let
		freeNamesInBody = freeNamesInMetaObject body
		typeParams' = map (substituteMetaObject subs) typeParams
		fieldTypes = map ($ typeParams') (fieldTypesOfSLCtorDefn ctor)
		(subs', fieldNames') = prepareForBindingNamesOfSLTerms freeNamesInBody fieldTypes (subs, fieldNames)
		body' = substituteMetaObject subs' body
		in (ctor, typeParams', fieldNames', body')
		| (ctor, typeParams, fieldNames, body) <- clauses]
	in MOSLTermCase subject' clauses'
substituteMetaObject subs (MOJSExprLiteral equiv type_ expr bindings) = let
	equiv' = substituteMetaObject subs equiv
	type_' = substituteMetaObject subs type_
	bindings' = M.map (\(JSExprBinding params value) -> let
		freeNamesInValue = freeNamesInMetaObject value
		(slParamNames, slParamTypes, jsParamNames, jsParamTypes) = unzip4 [
			(n1, t1, n2, t2)
			| JSExprBindingParam n1 t1 n2 t2 <- params]
		slParamTypes' = map (substituteMetaObject subs) slParamTypes
		fullSLParamTypes = [MTSLTerm t | t <- slParamTypes']
		(subs', slParamNames') = prepareForBindingNames freeNamesInValue fullSLParamTypes (subs, slParamNames)
		jsParamTypes' = map (substituteMetaObject subs') jsParamTypes
		fullJSParamTypes = [MTJSExpr t2 (MOName n1 t1) | (t2, n1, t1) <- zip3 jsParamTypes' slParamNames' fullSLParamTypes]
		(subs'', jsParamNames') = prepareForBindingNames freeNamesInValue fullJSParamTypes (subs, jsParamNames)
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

-- `prepareForBindingNames` is a helper function used when performing substitutions on a meta-type or meta-object which
-- introduces one or more new variables into scope. `names` are the new variables being introduced into scope, and
-- `nameTypes` are their meta-types. `freeNamesWithin` are the variables that are free in the part of the term where
-- `names` are in scope. `subs` are the substitutions to be performed. The return value is new values for `subs` and
-- `names`. It performs two jobs:
--  1. It correctly implements name shadowing by removing `names` from `subs` if they appear there
--  2. It implements capture-avoiding substitution by checking whether any of the values of the `subs` map contain
--     names from `names`, and changing that part of `names` to an unused variable if so. If it is necessary to change
--     `names`, it will also make a new entry in `subs` to perform the change.

prepareForBindingNames :: FreeNames
                       -> [MetaType]
                       -> (Substitutions, [Name])
                       -> (Substitutions, [Name])
prepareForBindingNames freeNamesWithin nameTypes (subs, names) = let
	subs' = subs { nameSubstitutions = foldr M.delete (nameSubstitutions subs) names }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> namesInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameSubstitutions subs'))
				| name <- S.toList (namesInFreeNames freeNamesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> namesInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameOfSLTermSubstitutions subs'))
				| name <- S.toList (namesOfSLTermsInFreeNames freeNamesWithin)]
	processNames :: [Name] -> [(Name, MetaType)] -> Substitutions -> (Substitutions, [Name])
	processNames processed [] innerSubs = (innerSubs, processed)
	processNames processed ((name, nameType):toProcess) innerSubs = let
		forbidden = incomingNames
			`S.union` S.delete name ((S.\\)
				(namesInFreeNames freeNamesWithin)
				(S.fromList (M.keys (nameSubstitutions innerSubs)))
				)
			`S.union` S.fromList processed
		candidates = [Name (unName name ++ replicate n '\'') | n <- [0..]]
		Just name' = find (`S.notMember` forbidden) candidates
		innerSubs' = if name' == name
			then innerSubs
			else innerSubs { nameSubstitutions = M.insert name (MOName name' nameType) (nameSubstitutions innerSubs) }
		in processNames (processed ++ [name']) toProcess innerSubs'
	in processNames [] (zip names nameTypes) subs'

-- `prepareForBindingNamesOfSLTerms` is like `prepareForBindingNames` except that it's for terms which introduce new
-- names into SL's term scope.

prepareForBindingNamesOfSLTerms :: FreeNames
                                -> [MetaObject]
                                -> (Substitutions, [NameOfSLTerm])
                                -> (Substitutions, [NameOfSLTerm])
prepareForBindingNamesOfSLTerms freeNamesWithin nameTypes (subs, names) = let
	subs' = subs { nameOfSLTermSubstitutions = foldr M.delete (nameOfSLTermSubstitutions subs) names }
	incomingNames =
			S.unions [
				maybe S.empty (\mo -> namesOfSLTermsInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameSubstitutions subs'))
				| name <- S.toList (namesInFreeNames freeNamesWithin)]
		`S.union`
			S.unions [
				maybe S.empty (\mo -> namesOfSLTermsInFreeNames (freeNamesInMetaObject mo))
					(M.lookup name (nameOfSLTermSubstitutions subs'))
				| name <- S.toList (namesOfSLTermsInFreeNames freeNamesWithin)]
	processNames :: [NameOfSLTerm] -> [(NameOfSLTerm, MetaObject)] -> Substitutions -> (Substitutions, [NameOfSLTerm])
	processNames processed [] innerSubs = (innerSubs, processed)
	processNames processed ((name, nameType):toProcess) innerSubs = let
		forbidden = incomingNames
			`S.union` S.delete name ((S.\\)
				(namesOfSLTermsInFreeNames freeNamesWithin)
				(S.fromList (M.keys (nameOfSLTermSubstitutions innerSubs)))
				)
			`S.union` S.fromList processed
		candidates = [NameOfSLTerm (unNameOfSLTerm name ++ replicate n '\'') | n <- [0..]]
		Just name' = find (`S.notMember` forbidden) candidates
		innerSubs' = if name' == name
			then innerSubs
			else innerSubs { nameOfSLTermSubstitutions = M.insert name (MOSLTermName name' nameType) (nameOfSLTermSubstitutions innerSubs) }
		in processNames (processed ++ [name']) toProcess innerSubs'
	in processNames [] (zip names nameTypes) subs'
