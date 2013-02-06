module Metacompiler.Reduce where

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
	MOJSExprLiteral equiv type_ expr bindings = runIdentity (traverseMetaObject reductionVisitor obj)

	-- If a binding hasn't resolved to a `MOJSExprLiteral`, it stays the way it is
	oldBindings :: M.Map (JS.Id ()) JSExprBinding
	oldBindings = M.filter (\b -> case valueOfJSExprBinding b of
		MOJSExprLiteral -> False
		_ -> True
		) bindings

	-- If a binding has resolved to a `MOJSExprLiteral`, collapse it into the parent `MOJSExprLiteral`.
	subs :: [(JS.Id (), 

	subsOrBindings' :: [Either (JS.Id (), 
	subsOrBindings' = 

reduceMetaObject other = runIdentity (traverseMetaObject reductionVisitor other)

reductionVisitor :: Visitor Identity
reductionVisitor = Visitor {
	visitMetaType = Identity . reduceMetaType,
	visitMetaObject = Identity . reduceMetaObject
	}

