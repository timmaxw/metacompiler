module Metacompiler.TLRuntime.Reduce where

import Control.Exception (assert)
import Control.Monad.Identity
import Control.Monad.State
import Data.List (find, tails)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.JS.JS as JS
import Metacompiler.SLRuntime.FreeNames as SLR
import Metacompiler.SLRuntime.Substitute as SLR
import Metacompiler.SLRuntime.TypeOf as SLR
import Metacompiler.SLRuntime.Types as SLR
import Metacompiler.TLRuntime.FreeNames
import Metacompiler.TLRuntime.Substitute
import Metacompiler.TLRuntime.Traverse
import Metacompiler.TLRuntime.Types

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
			substituteMetaObject (M.singleton paramName arg') body
		_ -> MOApp fun' arg'

-- TODO: Handle things like `(sl-term "x" (term "x" = ...))`. Also take into account the case where `x` takes
-- parameters, and make it work for all of `sl-term`, `sl-type`, and `js-expr`.

reduceMetaObject (MOSLType type_ bindings) = let
	type_' = Left type_
	bindings' = M.mapKeys Left (M.map Left bindings)
	(type_'', bindings'') = reduceBindings bindingReductionParametersForSL (type_', bindings')
	Left type_''' = type_''
	bindings''' = M.mapKeys (\ (Left x) -> x) (M.map (\ (Left x) -> x) bindings'')
	in MOSLType type_''' bindings'''

reduceMetaObject (MOSLTerm term typeBindings termBindings) = let
	term' = Right term
	bindings' = M.mapKeys Left (M.map Left typeBindings) `M.union` M.mapKeys Right (M.map Right termBindings)
	(term'', bindings'') = reduceBindings bindingReductionParametersForSL (term', bindings')
	Right term''' = term''
	typeBindings''' = M.fromList [(n, v) | (Left n, Left v) <- M.toList bindings'']
	termBindings''' = M.fromList [(n, v) | (Right n, Right v) <- M.toList bindings'']
	in MOSLTerm term''' typeBindings''' termBindings'''

reduceMetaObject (MOJSExprLiteral equiv type_ expr bindings) = let
	equiv' = reduceMetaObject equiv
	type_' = reduceMetaObject type_
	(expr', bindings') = reduceBindings bindingReductionParametersForJS (expr, bindings)
	in MOJSExprLiteral equiv' type_' expr' bindings'

reduceMetaObject (MOJSExprConvertEquiv newEquiv content) = case reduceMetaObject content of
	MOJSExprLiteral equiv type_ code bindings ->
		MOJSExprLiteral (reduceMetaObject newEquiv) type_ code bindings
	other -> MOJSExprConvertEquiv (reduceMetaObject newEquiv) other

reduceMetaObject other = runIdentity (traverseMetaObject reductionVisitor other)

reductionVisitor :: Visitor Identity
reductionVisitor = Visitor {
	visitMetaType = Identity . reduceMetaType,
	visitMetaObject = Identity . reduceMetaObject
	}

bindingReductionParametersForSL :: BindingReductionParameters
		(Either SLR.NameOfType SLR.NameOfTerm)
		(Either SLR.Type SLR.Term)
		(Either SLR.Kind SLR.Type)
		(Either SLTypeBinding SLTermBinding)
		(NameOfMetaObject, MetaObject)

bindingReductionParametersForSL = BindingReductionParameters {
	generateUniqueNameBRP = (\ name taken -> let
		unwrap :: Either SLR.NameOfType SLR.NameOfTerm -> String
		unwrap (Left (SLR.NameOfType n)) = n
		unwrap (Right (SLR.NameOfTerm n)) = n
		takenStrings = S.map unwrap taken
		candidateStrings = [unwrap name ++ replicate i '\'' | i <- [0..]]
		Just newNameString = find (`S.notMember` takenStrings) candidateStrings
		in case name of
			Left _ -> Left (SLR.NameOfType newNameString)
			Right _ -> Right (SLR.NameOfTerm newNameString)
		),
	makeBindingBRP = (\ name (params, value) -> case name of
		Left _ -> assert (null params) $ Left (SLTypeBinding value)
		Right _ -> Right (SLTermBinding params value)
		),
	unmakeBindingBRP = (\ binding -> case binding of
		Left (SLTypeBinding value) -> ([], value)
		Right (SLTermBinding params value) -> (params, value)
		),
	isMetaObjectALiteralBRP = (\ obj -> case obj of
		MOSLType type_ bindings -> Just (
			Left type_,
			M.mapKeys Left (M.map Left bindings)
			)
		MOSLTerm term typeBindings termBindings -> Just (
			Right term,
			M.mapKeys Left (M.map Left typeBindings) `M.union` M.mapKeys Right (M.map Right termBindings)
			)
		_ -> Nothing
		),
	primaryParamNameBRP = (\ (name, _) -> name),
	allParamNamesBRP = (\ (name, _) -> S.singleton name),
	reduceParamBRP = (\ (name, type_) -> (name, reduceMetaObject type_)),
	applySubstBRP = (\ typeOrTermName numParams newNames substFun typeOrTerm -> let
		(typeSubs, termSubs) = case typeOrTermName of
			Left typeName | numParams == 0 -> (
				M.singleton typeName (let Left value = substFun [] in SLR.simpleTypeSub value),
				M.empty
				)
			Right termName -> (
				M.empty,
				M.singleton termName (SLR.TermSub
					(S.fromList [name | Right name <- S.toList newNames])
					(\ params -> assert (length params >= numParams) $ let
						Right value = substFun (map Right $ take numParams params)
						in Identity (foldl SLR.TermApp value (drop numParams params)))
					)
				)
		in case typeOrTerm of
			Left type_ -> Left (runIdentity (SLR.substituteType typeSubs type_))
			Right term -> Right (runIdentity (SLR.substituteTerm (typeSubs, termSubs) term))
		),
	typeOfSubstInTermBRP = (\ typeOrTerm typeOrTermName binding -> let
		(kindsOfTypes, typesOfTerms) = case typeOrTerm of
			Left type_ -> (SLR.freeVarsInType type_, M.empty)
			Right term -> SLR.freeVarsInTerm term
		stripType :: Int -> SLR.Type -> SLR.Type
		stripType 0 t = t
		stripType i (SLR.TypeFun _ r) = stripType (i - 1) r
		in case (typeOrTermName, binding) of
			(Left typeName, Left (SLTypeBinding _)) ->
				Left (kindsOfTypes M.! typeName)
			(Right termName, Right (SLTermBinding params _)) ->
				Right (stripType (length params) (typesOfTerms M.! termName))
		),
	freeVarsAndGlobalsOfTermBRP = (\ typeOrTerm -> case typeOrTerm of
		Left type_ -> S.map Left (SLR.freeVarsAndGlobalsInType type_)
		Right term -> let
			(typeVars, termVars) = SLR.freeVarsAndGlobalsInTerm term
			in S.map Left typeVars `S.union` S.map Right termVars
		),
	freeVarsAndGlobalsInTypeBRP = (\ kindOrType -> case kindOrType of
		Left _ -> S.empty
		Right type_ -> S.map Left (SLR.freeVarsAndGlobalsInType type_)
		),
	makeTermInvokingSubstBRP = (\ typeOrTermName kindOrType typeOrTermArgs -> case (typeOrTermName, kindOrType) of
		(Left name, Left kind) -> let
			args = [t | t' <- typeOrTermArgs, let Left t = t']
			kind' = foldr SLR.KindFun kind (map SLR.kindOfType args)
			in Left (foldl SLR.TypeApp (SLR.TypeName name kind') args)
		(Right name, Right type_) -> let
			args = [t | t' <- typeOrTermArgs, let Right t = t']
			type_' = foldr SLR.TypeFun type_ (map SLR.typeOfTerm args)
			in Right (foldl SLR.TermApp (SLR.TermName name type_') args)
		)
	}

bindingReductionParametersForJS :: BindingReductionParameters
		(JS.Id ())
		(JS.Expression ())
		()
		JSExprBinding
		JSExprBindingParam

bindingReductionParametersForJS = BindingReductionParameters {
	generateUniqueNameBRP = (\ name taken -> let
		candidates = [JS.Id () (JS.unId name ++ replicate i '_') | i <- [0..]]
		Just name' = find (`S.notMember` taken) candidates
		in name'
		),
	makeBindingBRP = (\ _ (params, value) -> JSExprBinding params value),
	unmakeBindingBRP = (\ (JSExprBinding params value) -> (params, value)),
	isMetaObjectALiteralBRP = (\ obj -> case obj of
		MOJSExprLiteral _ _ expr bindings -> Just (expr, bindings)
		_ -> Nothing
		),
	primaryParamNameBRP = (\ (JSExprBindingParam _ _ n _) -> n),
	allParamNamesBRP = (\ (JSExprBindingParam n1 _ n2 _) -> S.fromList [n1, n2]),
	reduceParamBRP = (\ (JSExprBindingParam n1 t1 n2 t2) ->
		JSExprBindingParam n1 (reduceMetaObject t1) n2 (reduceMetaObject t2)),
	applySubstBRP = (\ name numParams namesIntroduced substFun expr ->
		JS.substituteExpression
			(M.singleton name
				(JS.SubstFun (\ params ->
					if length params == numParams
						then substFun params
						else error "wrong number of parameters to JS substitution"
				) namesIntroduced))
			expr
		),
	typeOfSubstInTermBRP = (\ _ _ _ -> ()),
	freeVarsAndGlobalsOfTermBRP = JS.freeNamesInExpression,
	freeVarsAndGlobalsInTypeBRP = const S.empty,
	makeTermInvokingSubstBRP = (\ name () args ->
		JS.CallExpr () (JS.VarRef () name) args
		)
	}

data BindingReductionParameters nameType termType typeType bindingType paramType = BindingReductionParameters {
	generateUniqueNameBRP :: nameType -> S.Set nameType -> nameType,

	makeBindingBRP :: nameType -> ([paramType], MetaObject) -> bindingType,
	unmakeBindingBRP :: bindingType -> ([paramType], MetaObject),

	isMetaObjectALiteralBRP :: MetaObject -> Maybe (termType, M.Map nameType bindingType),

	primaryParamNameBRP :: paramType -> NameOfMetaObject,
	allParamNamesBRP :: paramType -> S.Set NameOfMetaObject,
	reduceParamBRP :: paramType -> paramType,
	
	applySubstBRP :: nameType -> Int -> S.Set nameType -> ([termType] -> termType) -> termType -> termType,
	typeOfSubstInTermBRP :: termType -> nameType -> bindingType -> typeType,
	freeVarsAndGlobalsOfTermBRP :: termType -> S.Set nameType,
	freeVarsAndGlobalsInTypeBRP :: typeType -> S.Set nameType,
	makeTermInvokingSubstBRP :: nameType -> typeType -> [termType] -> termType
	}

{- `reduceBindings` is used for simplifying all types of literal meta-objects: SL terms and types, and JavaScript
expressions.

`reduceBindings` is very complicated. To explain how it works more clearly, I will refer back to the following example
in the comments in `reduceBindings`:

	(sl-term
		"globalFun . (subA . g1 g2) (subA . g3 g4) (subB . g5 g6) (subC . g7)"
		(term "subA" (param1 :: sl-term (sl-type "Nat")) (param2 :: sl-term (sl-type "Nat"))) =
			(sl-term "add . x y"
				(term "x" = param1)
				(term "y" = someFunction1 param2)
				)
			)
		(term "subB" (param1 :: sl-term (sl-type "Nat")) (param2 :: sl-term (sl-type "Nat")) =
			someFunction2 param1
			)
		(term "subC" (param :: sl-term (sl-type "Nat")) =
			param
			)
		(term "subD" (param :: sl-term (sl-type "Nat")) =
			someFunction3 param
			)
		)

This ought to be reduced to:

	(sl-term
		"globalFun (add . g1 (y . g2)) (add . g3 (y . g4)) (subB . g5 g6) g7"
		(term "subB" (param1 :: sl-term (sl-type "Nat")) =
			someFunction2 param1
			)
		(term "y" (param2 :: sl-term (sl-type "Nat")) =
			someFunction1 param2
			)
		)

-}

reduceBindings :: Ord nameType
               => BindingReductionParameters nameType termType typeType bindingType paramType
               -> (termType, M.Map nameType bindingType)
               -> (termType, M.Map nameType bindingType)
reduceBindings brp (originalOuterTerm, originalOuterBindings) = let

	-- `originalOuterTerm` is the term of the meta-object literal, and `originalOuterBindings` is the bindings that
	-- come with the meta-object literal
	
	-- In our example, `originalOuterTerm` would be:
	--     globalFun . (subA . g1 g2) (subA . g3 g4) (subB . g5 g6) (subC . g7)
	-- The keys of `originalOuterBindings` would be `subA`, `subB`, `subC`, and `subD`.

	-- `filterByFlags` takes a list of objects and a list of `Bool`s saying whether to keep each one.
	filterByFlags :: [Bool] -> [a] -> [a]
	filterByFlags [] [] = []
	filterByFlags (True:flags) (a:objs) = a : filterByFlags flags objs
	filterByFlags (False:flags) (_:objs) = filterByFlags flags objs

	-- `reducedOuterBindings` is `originalOuterBindings` but with basic reductions performed on each binding. In our
	-- example, `reducedOuterBindings` would be the same as `originalOuterBindings`, because all of the values of the
	-- bindings are already in their simplest forms.
	reducedOuterBindings = M.mapWithKey
		(\ name binding -> let
			(params, value) = unmakeBindingBRP brp binding
			params' = map (reduceParamBRP brp) params
			value' = reduceMetaObject value
			in makeBindingBRP brp name (params', value'))
		originalOuterBindings

	-- `globals` is all of the global variables that are mentioned in both the original outer term and the things that
	-- will get substituted into it. In our example, it contains the names:
	--     globalFun g1 g2 g3 g4 g5 g6 g7 add Nat
	-- The reason why it contains `Nat` is that variables in an `SLTerm` object carry their types along with them, even
	-- though this is not visible when they are written out in SL's S-expression syntax.
	globals = (freeVarsAndGlobalsOfTermBRP brp originalOuterTerm S.\\ S.fromList (M.keys reducedOuterBindings))
		`S.union` S.unions [
			case isMetaObjectALiteralBRP brp value of
				Nothing -> S.empty
				Just (innerTerm, innerBindings) ->
					freeVarsAndGlobalsOfTermBRP brp innerTerm S.\\ S.fromList (M.keys innerBindings)
			| (_, binding) <- M.toList reducedOuterBindings
			, let (_, value) = unmakeBindingBRP brp binding]

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name. There are no
	-- name conflicts in our example, so `uniqify` will always return the unmodified name.

	-- uniqify :: nameType -> State (S.Set nameType) nameType
	uniqify name = do
		takenNames <- get
		let name' = generateUniqueNameBRP brp name takenNames
		put (S.insert name' takenNames)
		return name'

	-- `processOuterBindings` is the core of the algorithm. It iterates over all of the bindings one by one and deals
	-- with each one, either by leaving it as a binding, folding it into the outer term, or getting rid of it.

	-- processOuterBindings :: (termType, [(nameType, bindingType)])
	--                      -> State (S.Set nameType) (termType, [(nameType, bindingType)])

	processOuterBindings (outerTerm, []) = do
		return (outerTerm, [])

	processOuterBindings (outerTerm, (outerBindingName, outerBinding):remainingOuterBindings) = let

		-- In our example, this code path runs once for each of `subA`, `subB`, `subC`, and `subD`.

		(outerBindingParams, outerBindingValue) = unmakeBindingBRP brp outerBinding

		-- `paramPrimaryNames` tells which param introduces each variable into scope. For example, for `subA` it would
		-- map `param1` to `0` and `param2` to `1`.

		-- paramPrimaryNames :: M.Map NameType Integer
		paramPrimaryNames = M.fromList [
			(name, i)
			| (i, param:rest) <- zip [0..] (tails outerBindingParams)
			, let name = primaryParamNameBRP brp param
			, name `S.notMember` S.unions (map (allParamNamesBRP brp) rest)]

		in case (outerBindingValue, isMetaObjectALiteralBRP brp outerBindingValue) of

			(_, _) | outerBindingName `S.notMember` freeVarsAndGlobalsOfTermBRP brp outerTerm -> do
				-- This binding isn't used at all. We should just ignore it.
			
				-- In our example, the `subD` binding will be handled by this code path.

				processOuterBindings (outerTerm, remainingOuterBindings)

			(MOName name _, _) | name `M.member` paramPrimaryNames -> do
				-- The binding is simply the name of one of its parameters, so we can easily reduce it.

				-- In our example, the `subC` binding will be handled by this code path. `numberOfTheParam` will be
				-- `0`, because `param` is the first parameter to `subC`.

				let numberOfTheParam = paramPrimaryNames M.! name

				let outerTerm' = applySubstBRP brp
						outerBindingName
						(length outerBindingParams)
						S.empty
						(!! numberOfTheParam)
						outerTerm

				processOuterBindings (outerTerm', remainingOuterBindings)

			(_, Nothing) -> do
				-- This binding is used, but cannot be reduced further, so we must keep it. However, we might still
				-- have to rename it so it doesn't conflict with global names, and we might want to prune unused
				-- parameters.

				-- In our example, the `subB` binding will be handled by this code path.

				outerBindingName' <- uniqify outerBindingName

				-- It might be that the binding doesn't use all of its parameters. This list has `True` for every one
				-- that is used and `False` for every one that is no longer needed.

				-- In our example, we want to get rid of `param2` to the `subB` binding because it isn't used, so 
				-- `outerBindingParamsKeepFlags` would be `[True, False]`.
				let
					outerBindingParamsKeepFlags :: [Bool]
					outerBindingParamsKeepFlags = [
						or [
							-- This condition tests that the parameter's name appears in the expression.
							-- In our example, this test would fail for `param2`.
							name `M.member` freeVarsInMetaObject outerBindingValue
							-- This condition tests that the parameter isn't being shadowed by another parameter.
							&& name `S.notMember` S.unions (map (allParamNamesBRP brp) rest)
							| name <- S.toList (allParamNamesBRP brp param)]
						| param:rest <- tails outerBindingParams]

				let outerBindingParams' = filterByFlags outerBindingParamsKeepFlags outerBindingParams

				let outerBinding' = makeBindingBRP brp outerBindingName (outerBindingParams', outerBindingValue)

				-- In our example for `subB`, `bindingType` would be `Nat`.
				let bindingType = typeOfSubstInTermBRP brp outerTerm outerBindingName outerBinding

				-- In our example, `outerTerm'` would be the same as `outerTerm` except that `subB global5 global6` is
				-- replaced with `subB global5`.
				let outerTerm' = applySubstBRP brp
					outerBindingName
					(length outerBindingParams)
					(S.singleton outerBindingName' `S.union` freeVarsAndGlobalsInTypeBRP brp bindingType)
					(\ outerBindingParamsValues -> makeTermInvokingSubstBRP brp
						outerBindingName'
						bindingType
						(filterByFlags outerBindingParamsKeepFlags outerBindingParamsValues))
					outerTerm

				(finishedOuterTerm, remainingOuterBindings') <-
					processOuterBindings (outerTerm', remainingOuterBindings)

				let finishedOuterBindings = (outerBindingName', outerBinding') : remainingOuterBindings'

				return (finishedOuterTerm, finishedOuterBindings)
		
			(_, Just (originalInnerTerm, originalInnerBindings)) -> do
				-- This binding evaluates to another "inner" literal, so we can fold it into the outer literal.

				-- In our example, this clause is used to handle the `subA` binding. `originalInnerTerm` would be
				-- `add . x y`, and `originalInnerBindings` would have two keys, `x` and `y`.

				-- `processInnerBindings` iterates over the bindings on the inner literal. For an explanation of its
				-- return value, look at the place where it is called.
				let
					-- processInnerBindings :: [(nameType, bindingType)]
					--                      -> ([termType] -> termType)
					--                      -> State (S.Set nameType) (
					--                             [termType] -> termType,
					--                             [(nameType, bindingType)]
					--                             )

					processInnerBindings [] innerTermFun = do
						return (innerTermFun, [])

					processInnerBindings ((innerBindingName, innerBinding):remainingInnerBindings) innerTermFun = let

						-- In our example, we will take this code path twice: once for `x` and once for `y`. In both
						-- cases, `innerBindingParams` is empty. For `x`, `innerBindingValue` is `param1`; for `y`,
						-- `innerBindingValue` is `someFunction1 param2`.

						-- innerBindingParams :: [(nameType, bindingType)]
						-- innerBindingValue :: MetaObject
						(innerBindingParams, innerBindingValue) = unmakeBindingBRP brp innerBinding

						in case innerBindingValue of

							MOName name _ | name `M.member` paramPrimaryNames -> do

								-- This inner binding's value is just a variable from the outer binding. It will be
								-- collapsed into the outer literal's term instead of becoming a binding on the outer
								-- literal.

								-- In our example, we use this code path to handle the binding `x`.

								let numberOfTheParam = paramPrimaryNames M.! name

								let
									-- innerTermFun' :: [termType] -> termType
									innerTermFun' outerBindingParamValues = let
										innerTerm = innerTermFun outerBindingParamValues
										valueOfTheParam = outerBindingParamValues !! numberOfTheParam
										in applySubstBRP brp
											innerBindingName
											(length innerBindingParams)
											(freeVarsAndGlobalsOfTermBRP brp valueOfTheParam)
											(const valueOfTheParam)
											innerTerm

								processInnerBindings remainingInnerBindings innerTermFun'

							_ -> do

								-- This inner binding's value is something more complicated. It's not a variable of the
								-- outer binding, or the above case would have caught it; and it's not another literal,
								-- or it would have been collapsed into the inner term when we made
								-- `reducedOuterBindings`. So it will have to become a new outer binding.

								-- In our example, we use this code path to handle the binding `y`.

								newOuterBindingName <- uniqify innerBindingName

								-- It might be that not all of the outer binding parameters are used in this inner binding.
								-- This list has `True` for every one that is used and `False` for every one that is no
								-- longer needed.
								let
									outerBindingParamsKeepFlags :: [Bool]
									outerBindingParamsKeepFlags = [
										or [
											-- This condition tests that the parameter's name appears in the expression.
											name `M.member` freeVarsInMetaObject innerBindingValue
											-- This condition tests that the parameter isn't being shadowed by another
											-- parameter.
											&& name `S.notMember` S.unions (map (allParamNamesBRP brp) rest)
											| name <- S.toList (allParamNamesBRP brp param)]
										| param:rest <- tails outerBindingParams]

								let outerBindingParamsKept = filterByFlags outerBindingParamsKeepFlags outerBindingParams
								let newOuterBindingParams = outerBindingParamsKept ++ innerBindingParams

								let newOuterBinding = makeBindingBRP brp
										innerBindingName
										(newOuterBindingParams, outerBindingValue)

								let
									-- innerTermFun' :: [termType] -> termType
									innerTermFun' outerBindingParamValues = let
										innerTerm = innerTermFun outerBindingParamValues
										prunedParamValues = filterByFlags outerBindingParamsKeepFlags outerBindingParamValues
										bindingType = typeOfSubstInTermBRP brp innerTerm innerBindingName innerBinding
										in applySubstBRP brp
											innerBindingName
											(length innerBindingParams)
											(S.singleton newOuterBindingName
												`S.union` freeVarsAndGlobalsInTypeBRP brp bindingType
												`S.union` S.unions (map (freeVarsAndGlobalsOfTermBRP brp) prunedParamValues))
											(\ innerBindingParamValues -> makeTermInvokingSubstBRP brp
												newOuterBindingName
												bindingType
												(prunedParamValues ++ innerBindingParamValues)
												)
											innerTerm

								(finalInnerTermFun, outerBindingsFromRemainingInnerBindings) <-
									processInnerBindings remainingInnerBindings innerTermFun'

								let finalOuterBindings =
										(newOuterBindingName, newOuterBinding) : outerBindingsFromRemainingInnerBindings

								return (finalInnerTermFun, finalOuterBindings)

				-- `innerTermFun` is a function that is called for every place in the outer term where the outer
				-- binding is referenced. The parameters are the terms passed to that invocation of the binding, and
				-- the return value is the term to replace that invocation with.

				-- `newOuterBindings` is a list of all the bindings from the inner literal that need to be copied over
				-- to the outer literal.

				-- In our example, `innerTermFun` will be called twice:
				-- * With parameters `g1` and `g2`, and return value `add . g1 (y . g2)`
				-- * With parameters `g3` and `g4`, and return value `add . g3 (y . g4)`
				-- `newOuterBindings` would have only one element, which is the binding named `y`.

				-- innerTermFun :: [termType] -> termType
				-- newOuterBindings :: [(nameType, bindingType)]
				(innerTermFun, newOuterBindings) <-
					processInnerBindings (M.toList originalInnerBindings) (const originalInnerTerm)

				let newVarsIntroduced =
						(freeVarsAndGlobalsOfTermBRP brp originalInnerTerm
							S.\\ S.fromList (M.keys originalInnerBindings))
						`S.union` S.fromList (map fst newOuterBindings)
				let outerTerm' = applySubstBRP brp
						outerBindingName
						(length outerBindingParams)
						newVarsIntroduced
						innerTermFun
						outerTerm

				(finishedOuterTerm, remainingOuterBindings') <-
					processOuterBindings (outerTerm', remainingOuterBindings)

				let finishedOuterBindings = newOuterBindings ++ remainingOuterBindings'

				return (finishedOuterTerm, finishedOuterBindings)

	-- In our example, `finalOuterTerm` would be:
	--     globalFun (add . g1 (y . g2)) (add . g3 (y . g4)) (subB . g5) g7
	-- and `finalOuterBindings` would be a list of two bindings, named `subB` and `y`.

	-- finalOuterTerm :: termType
	-- finalOuterBindings :: [(nameType, bindingType)]
	(finalOuterTerm, finalOuterBindings) =
		flip evalState globals $
			processOuterBindings (originalOuterTerm, M.toList reducedOuterBindings)

	in (finalOuterTerm, M.fromList finalOuterBindings)


