module Metacompiler.TLRuntime.FreeNames where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Metacompiler.TLRuntime.Traverse
import Metacompiler.TLRuntime.Types

-- `freeVarsInMetaType` and `freeVarsInMetaObject` find all the unbound variables in the given term
-- and return their names and types.

freeVarsInMetaType :: MetaType -> M.Map NameOfMetaObject MetaType
freeVarsInMetaType (MTFun (paramName, paramType) resultType) = let
	paramNames = freeVarsInMetaType paramType
	resultNames = M.delete paramName (freeVarsInMetaType resultType)
	in paramNames `M.union` resultNames
freeVarsInMetaType other =
	getConst (traverseMetaType freeVarsVisitor other)

freeVarsInMetaObject :: MetaObject -> M.Map NameOfMetaObject MetaType
freeVarsInMetaObject (MOAbs (paramName, paramType) body) = let
	paramNames = freeVarsInMetaType paramType
	bodyNames = M.delete paramName (freeVarsInMetaObject body)
	in paramNames `M.union` bodyNames'
freeVarsInMetaObject (MOName n type_) =
	M.singleton n type_ `M.union` freeVarsInMetaType type_
freeVarsInMetaObject (MOSLType _ bindings) =
	M.unions (map freeVarsInSLTypeBinding bindings)
freeVarsInMetaObject (MOSLTerm _ typeBindings termBindings) =
	M.unions (map freeVarsInSLTypeBinding typeBindings)
	`M.union` M.unions (map freeVarsInSLTermBinding termBindings)
freeVarsInMetaObject (MOJSExprLiteral equiv type_ _ bindings) =
	freeVarsInMetaObject equiv
	`M.union` freeVarsInMetaObject type_
	`M.union` M.unions (map freeVarsInJSExprBinding bindings)
freeVarsInMetaObject other =
	getConst (traverseMetaObject freeVarsVisitor other)

freeVarsInSLTypeBinding :: SLTypeBinding -> M.Map NameOfMetaObject MetaType
freeVarsInSLTypeBinding (SLTypeBinding value) =
	freeVarsInMetaObject value

freeVarsInSLTermBinding :: SLTermBinding -> M.Map NameOfMetaObject MetaType
freeVarsInSLTermBinding (SLTermBinding params value) =
	M.unions [freeVarsInMetaObject t | (_, t) <- params]
	`M.union` foldr M.delete (freeVarsInMetaObject value) [n | (n, _) <- params]

freeVarsInJSExprBinding :: JSExprBinding -> M.Map NameOfMetaObject MetaType
freeVarsInJSExprBinding (JSExprBinding params value) =
	M.unions [freeVarsInMetaObject t1 `M.union` M.delete n1 (freeVarsInMetaObject t2)
		| JSExprBindingParam n1 t1 _ t2 <- params]
	`M.union` foldr M.delete (freeVarsInMetaObject value)
		(concat [[n1, n2] | JSExprBindingParam n1 _ n2 _ <- params])

freeVarsVisitor :: Visitor (Const (M.Map NameOfMetaObject MetaType))
freeVarsVisitor = Visitor {
	visitMetaType = Const . freeVarsInMetaType,
	visitMetaObject = Const . freeVarsInMetaObject
	}

-- `globalsInMetaType` and `globalsInMetaObject` return the names of all the global objects that the given type or term
-- refers to.

globalsInMetaType :: MetaType -> S.Set NameOfMetaObject
globalsInMetaType other =
	getConst (traverseMetaType globalsVisitor other)

globalsInMetaObject :: MetaObject -> S.Set NameOfMetaObject
globalsInMetaObject (MOJSExprTypeDefn defn params) =
	S.singleton defn `S.union` S.unions (map globalsInMetaObject params)
globalsInMetaObject other =
	getConst (traverseMetaObject globalsVisitor other)

globalsVisitor :: Visitor (Const Names)
globalsVisitor = Visitor {
	visitMetaType = Const . globalsInMetaType,
	visitMetaObject = Const . globalsInMetaObject
	}

-- Minor variations on the above functions

freeVarsAndGlobalsInMetaObject :: MetaObject -> S.Set NameOfMetaObject
freeVarsAndGlobalsInMetaObject o =
	S.fromList (M.keys freeVarsInMetaObject o) `S.union` globalsInMetaObject o

freeVarsAndGlobalsInMetaType :: MetaType -> S.Set NameOfMetaObject
freeVarsAndGlobalsInMetaType t =
	S.fromList (M.keys freeVarsInMetaType t) `S.union` globalsInMetaType t

