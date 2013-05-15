module Metacompiler.TLRuntime.Substitute where

import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Metacompiler.JS as JS
import Metacompiler.TLRuntime.FreeNames
import Metacompiler.TLRuntime.Traverse
import Metacompiler.TLRuntime.Types

-- `substituteMetaType` and `substituteMetaObject` traverse the given meta-type or meta-object; whenever they encounter
-- a reference to a free variable that appears in the given map, they replace it with its value from the map. They
-- correctly implement name shadowing and avoid capturing variables. For example, replacing `a` with `b` in
-- `fun (a :: ...) -> a` will leave it unchanged, and replacing `a` with `b` in `fun (b :: ...) -> a` will produce
-- `fun (b' :: ...) -> b`.

substituteMetaType :: M.Map NameOfMetaObject MetaObject -> MetaType -> MetaType
substituteMetaType subs (MTFun (paramName, paramType) returnType) = let
	paramType' = substituteMetaType subs paramType
	(paramName', [subs']) = prepareBinding (paramName, paramType') [(freeNamesInMetaType returnType, subs)]
	returnType' = substituteMetaType subs' returnType
	in MTFun (paramName', paramType') returnType'
substituteMetaType subs other = runIdentity (traverseMetaType (makeSubstitutionVisitor subs) other)

substituteMetaObject :: M.Map NameOfMetaObject MetaObject -> MetaObject -> MetaObject
substituteMetaObject subs (MOAbs (paramName, paramType) body) = let
	paramType' = substituteMetaType subs paramType
	(paramName', [subs']) = prepareBinding (paramName, paramType') [(freeNamesInMetaObject body, subs)]
	body' = substituteMetaObject subs' body
	in MOAbs (paramName', paramType') body'
substituteMetaObject subs (MOName name type_) = case M.lookup name subs of
	Just value -> value
	Nothing -> MOName name (substituteMetaType subs type_)
substituteMetaObject subs (MOSLType type_ typeBindings) =
	MOSLType type_
		(M.map (substituteSLTypeBinding subs) typeBindings)
substituteMetaObject subs (MOSLTerm term typeBindings termBindings) =
	MOSLTerm term
		(M.map (substituteSLTermBindings subs) typeBindings)
		(M.map (substituteSLTypeBindings subs) termBindings)
substituteMetaObject subs (MOJSExprTypeDefn defn params) =
	case M.lookup (nameOfJSExprTypeDefn defn) subs of
		Just _ -> error "trying to substitute the name of a global variable"
		Nothing -> MOJSExprTypeDefn defn (map (substituteMetaObject subs) params)
substituteMetaObject subs (MOJSExprLiteral equiv type_ expr bindings) =
	MOJSExprLiteral
		(substituteMetaObject subs equiv)
		(substituteMetaObject subs type_)
		(M.map (substituteJSExprBinding subs) bindings)
substituteMetaObject subs other =
	runIdentity (traverseMetaObject (makeSubstitutionVisitor subs) other)

substituteSLTypeBinding :: M.Map NameOfMetaObject MetaObject -> SLTypeBinding -> SLTypeBinding
substituteSLTypeBinding subs (SLTypeBinding value) =
	SLTypeBinding (substituteMetaObject subs value)

substituteSLTermBinding :: M.Map NameOfMetaObject MetaObject -> SLTermBinding -> SLTermBinding
substituteSLTermBinding subs (SLTermBinding params value) = (uncurry SLTermBinding) (f subs params)
	where
		f subs' [] = ([], substituteMetaObject subs' value)
		f subs' ((name, type_):otherParams) = let
			type_' = substituteMetaObject subs type_
			namesInRest = freeNamesInMetaObject value S.\\ S.fromList (map fst otherParams)
			(name', [subs'']) = prepareBinding (name, MTSLTerm type_') [(namesInRest, subs')] 
			(otherParams', value') = f subs'' otherParams
			in ((name', type_'):otherParams', value')

substituteJSExprBinding :: M.Map NameOfMetaObject MetaObject -> JSExprBinding -> JSExprBinding
substituteJSExprBinding subs (JSExprBinding params value) = (uncurry JSExprBinding) (f subs params)
	where
		f subs' [] = ([], substituteMetaObject subs' value)
		f subs' (JSExprBindingParam n1 t1 n2 t2 : otherParams) = let
			t1' = substituteMetaObject subs t1
			namesInT2 = freeNamesInMetaObject t2
			namesInRest = freeNamesInMetaObject value
				S.\\ S.fromList (concat [[a, b] | JSExprBindingParam a _ b _ <- otherParams])
			(n1', [t2subs, restSubs]) = prepareBinding (n1, MTSLTerm t1') [(namesInT2, subs), (S.delete n2 namesInRest, subs')]
			t2' = substituteMetaObject t2subs t2
			(n2', [restSubs']) = prepareBinding (n2, MTJSExpr t2' (MOName n1' t1')) [(namesInRest, restSubs)]
			(otherParams', value') = f restSubs' otherParams
			in ((JSExprBindingParam n1' t1' n2' t2'):otherParams', value')

makeSubstitutionVisitor :: M.Map NameOfMetaObject MetaObject -> Visitor Identity
makeSubstitutionVisitor subs = Visitor {
	visitMetaType = Identity . substituteMetaType subs,
	visitMetaObject = Identity . substituteMetaObject subs
	}

prepareBinding :: (NameOfMetaObject, MetaType)
               -> [(S.Set NameOfMetaObject, M.Map NameOfMetaObject MetaObject)]
               -> (NameOfMetaObject, [M.Map NameOfMetaObject MetaObject])
prepareBinding (name, nameType) contexts = let
	forbiddenNames = S.unions [
		S.unions [maybe S.empty freeAndGlobalNamesInMetaObject $ M.lookup n subs
			| n <- S.toList (S.delete name namesWithin)]
		`S.union` S.delete name namesWithin
		| (namesWithin, subs) <- contexts]
	candidateNames = [NameOfMetaObject (unNameOfMetaObject name ++ replicate i '\'') | i <- [0..]]
	Just name' = find (`S.notMember` forbiddenNames) candidateNames
	subsFun = if name' == name then S.delete name else S.insert name (MOName name' nameType)
	in (name', map (subsFun . snd) contexts)

