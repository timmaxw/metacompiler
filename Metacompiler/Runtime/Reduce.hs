module Metacompiler.Runtime.Reduce where

import Control.Monad.Identity
import qualified Data.Map as M
import qualified Metacompiler.JS as JS
import Metacompiler.Runtime.Substitute
import Metacompiler.Runtime.Traverse
import Metacompiler.Runtime.Types

-- `reduceMetaType` and `reduceMetaObject` return the simplest meta-type or meta-object equivalent to the given
-- meta-type or meta-object. They are idempotent. Note that they do not reduce SL terms because this might never
-- terminate.

reduceMetaType :: MetaType -> MetaType

reduceMetaType other = runIdentity (traverseMetaType reductionVisitor other)

reduceMetaObject :: MetaObject -> MetaObject

reduceMetaObject (MOApp fun arg) = let
	fun' = reduceMetaObject fun
	arg' = reduceMetaObject arg
	in case fun' of
		MOAbs (paramName, _) body -> reduceMetaObject $
			substituteMetaObject (Substitutions (M.singleton paramName arg') M.empty M.empty) body
		_ -> MOApp fun' arg'

reduceMetaObject obj@(MOJSExprLiteral _) = let
	-- This is a convenient way to reduce `equiv`, `type_`, and `bindings`
	obj'@(MOJSExprLiteral equiv type_ expr bindings) = runIdentity (traverseMetaObject reductionVisitor obj)
	in case tryReduceMetaObjectToJSExpression S.empty obj' of
		Nothing -> obj'
		Just fun -> MOJSExprLiteral equiv type_ (fun M.empty) M.empty

reduceMetaObject other = runIdentity (traverseMetaObject reductionVisitor other)

reductionVisitor :: Visitor Identity
reductionVisitor = Visitor {
	visitMetaType = Identity . reduceMetaType,
	visitMetaObject = Identity . reduceMetaObject
	}

tryReduceMetaObjectToJSExpression :: S.Set Name -> MetaObject -> Maybe (M.Map Name (JS.Expression ()) -> JS.Expression ())
tryReduceMetaObjectToJSExpression promised obj@(MOName n _) = if n `S.member` names
	then Just (\values -> (M.!) values n)
	else Nothing
tryReduceMetaObjectToJSExpression promised (MOJSExprLiteral _ _ expr bindings) = do
	reduced <- mapM (tryReduceJSExprBindingToJSSubst promised) bindings
	return (\valuesOfPromised -> JS.substituteExpression (M.map ($ valuesOfPromised) reduced) expr)

tryReduceJSExprBindingToJSSubst :: S.Set Name -> JSExprBinding -> Maybe (M.Map Name (JS.Expression ()) -> JS.Subst)
tryReduceJSExprBindingToJSSubst promised (JSExprBinding params value) = do
	let paramNames = [n | JSExprBindingParam _ _ n _ <- params]
	let promised' = promised `S.union` S.fromList paramNames
	valueAsFun <- tryReduceMetaObjectToJSExpression promised' value
	return (\valuesOfPromised -> let
		substFun = \ paramValues -> if length paramValues == length paramNames
			then valueAsFun (M.fromList (zip paramNames paramValues) `M.union` valuesOfPromised)
			else error "wrong number of parameters"
		dummyValues = M.fromList (zip paramNames (repeat (JS.NullLit ()))) `M.union` valuesOfPromised
		possibleVars = JS.freeVarsInExpression (valueAsFun dummyValues)
		in SubstFun substFun possibleVars
		)

