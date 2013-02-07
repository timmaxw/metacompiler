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
	obj'@(MOJSExprLiteral equiv type_ expr bindings) = runIdentity (traverseMetaObject reductionVisitor obj)

	tryReduce :: S.Set Name -> MetaObject -> Maybe (M.Map Name (JS.Expression ()) -> JS.Expression ())
	tryReduce promised obj@(MOName n _) = if n `S.member` names
		then Just (\values -> (M.!) values n)
		else Nothing
	tryReduce promised (MOJSExprLiteral equiv type_ expr bindings) = do
		reduced <- liftM M.fromList $ sequence [do
			let paramNames = [n | JSExprBindingParam _ _ n _ <- params]
			let promised' = promised `S.union` S.fromList paramNames
			value' <- tryReduce promised' value
			return (name, (paramNames, value'))
			| (name, JSExprBinding params value) <- M.toList bindings]
		return (\valuesFromAbove -> let
			subs = M.map (\ (paramNames, value') -> let
				fun' = \ paramValues -> if length paramValues == length paramNames
					then value' (M.fromList (zip paramNames paramValues) `M.union` valuesFromAbove)
					else error "wrong number of parameters"
				dummyValues = M.fromList [(pn, JS.NullLit ()) | pn <- paramNames] `M.union` valuesFromAbove
				possibleVars = JS.freeVarsInExpression (value' dummyValues)
				in SubstFun fun possibleVars
				)
			in substituteExpression subs expr)

	in case tryReduce S.empty obj' of
		Nothing -> obj'
		Just fun -> MOJSExprLiteral equiv type_ (fun M.empty) M.empty

reduceMetaObject other = runIdentity (traverseMetaObject reductionVisitor other)

reductionVisitor :: Visitor Identity
reductionVisitor = Visitor {
	visitMetaType = Identity . reduceMetaType,
	visitMetaObject = Identity . reduceMetaObject
	}

	
