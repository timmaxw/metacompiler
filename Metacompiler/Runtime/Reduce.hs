module Metacompiler.Runtime.Reduce where

import Control.Monad.Identity
import qualified Data.Map as M
import qualified Data.Set as S
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

reduceMetaObject obj@(MOSLType { }) = let
	-- This is a convenient way to reduce `bindings`
	MOSLType type_ bindings = runIdentity (traverseMetaObject reductionVisitor obj)
	(bindings', subs) = 

reduceMetaObject obj@(MOSLTerm { }) = let
	-- This is a convenient way to reduce `bindings`
	MOSLTerm term bindings = runIdentity (traverseMetaObject reductionVisitor obj)
	(bindings', subs) = mconcat $ (flip evalState) (S.fromList (M.keys bindings)) $ sequence [
		case value of
			MOSLTerm innerTerm innerBindings -> do
				 innerBindings' <- sequence [
				 	innerName' <- uniqifyName  innerName
				 	
				 	| (innerName, innerBinding) <- innerBindings]
			_ -> if ({- name appears in term -})
				then return (M.singleton name (SLTermBinding params value), M.empty)
				else return (M.empty, M.empty)
		| (name, SLTermBinding params value) <- M.toList bindings]

reduceMetaObject obj@(MOJSExprLiteral { }) = let
	-- This is a convenient way to reduce `equiv`, `type_`, and `bindings`
	obj'@(MOJSExprLiteral equiv type_ expr bindings) = runIdentity (traverseMetaObject reductionVisitor obj)
	in case tryReduceMetaObjectToJSExpression S.empty obj' of
		Nothing -> obj'
		Just fun -> MOJSExprLiteral equiv type_ (fun M.empty) M.empty

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
		(Either NameOfSLType NameOfSLTerm)
		(Either SLType SLTerm)
		(Either SLKind SLType)
		(Either SLTypeBinding SLTermBinding)
		(NameOfMetaObject, MetaObject)
		(Either SL.TypeSub SL.TermSub)

bindingReductionParametersForSL = BRP {
	}

data BindingReductionParameters nameType termType typeType bindingType paramType substType = BRP {
	generateUniqueNameBRP :: nameType -> S.Set nameType -> nameType,

	makeBindingBRP :: ([paramType], MetaObject) -> bindingType,
	unmakeBindingBRP :: bindingType -> ([paramType], MetaObject),

	isMetaObjectALiteralBRP :: MetaObject -> Maybe (termType, M.Map nameType bindingType),

	primaryParamNameBRP :: paramType -> NameOfMetaObject,
	allParamNamesBRP :: paramType -> S.Set NameOfMetaObject,
	reduceParamBRP :: paramType -> paramType,
	
	makeSubstBRP :: Int -> S.Set nameType -> ([termType] -> termType) -> substType,
	applySubstsBRP :: M.Map nameType substType -> termType -> termType,
	typeOfSubstInTermBRP :: termType -> nameType -> bindingType -> typeType,
	freeVarsAndGlobalsOfTermBRP :: termType -> S.Set nameType,
	freeVarsAndGlobalsOfTypeBRP :: typeType -> S.Set nameType,
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
               => BindingReductionParameters nameType termType typeType bindingType paramType substType
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
	reducedOuterBindings = M.map
		(\ binding -> let
			(params, value) = makeBindingBRP brp binding
			params' = map (reduceParamBRP brp) params
			value' = reduceMetaObject value
			in makeBindingBRP brp (params', value'))
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
			| (_, value) <- M.toList reducedOuterBindings]

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

			(_, _) | outerBindingName `S.notMember` freeVarsAndGlobalsOfTermBRP brp outerTerm = do
				-- This binding isn't used at all. We should just ignore it.
			
				-- In our example, the `subD` binding will be handled by this code path.

				processOuterBindings (outerTerm, remainingOuterBindings)

			(MOName name _, _) | name `M.member` paramPrimaryNames =
				-- The binding is simply the name of one of its parameters, so we can easily reduce it.

				-- In our example, the `subC` binding will be handled by this code path.

				let numberOfTheParam = paramPrimaryNames M.! name

				let subst = makeSubstBRP brp
						(length outerBindingParams)
						S.empty
						(!! numberOfTheParam)

				let outerTerm' = applySubstsBRP (M.singleton outerBindingName' subst) outerTerm

				processOuterBindings (outerTerm', remainingOuterBindings)

			Nothing -> do
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
							&& n `S.notMember` S.unions (map (allParamNamesBRP brp) rest)
							| name <- S.toList (allParamNamesBRP brp param)]
						| param:rest <- tails outerBindingParams]

				let outerBindingParams' = filterByFlags outerBindingParamsKeepFlags outerBindingParams

				let outerBinding' = makeBindingBRP brp (outerBindingParams', outerBindingValue)

				-- In our example for `subB`, `bindingType` would be `Nat`.
				let bindingType = typeOfSubstInTermBRP outerTerm outerBindingName outerBinding

				let subst = makeSubstBRP brp
					(length outerBindingParams)
					(S.singleton outerBindingName' `S.union` freeVarsAndGlobalsInTypeBRP brp bindingType)
					(\ outerBindingParamsValues -> makeTermInvokingSubstBRP brp
						outerBindingName'
						bindingType
						(filterByFlags outerBindingParamsKeepFlags outerBindingParamsValues))

				-- In our example, `outerTerm'` would be the same as `outerTerm` except that `subB global5 global6` is
				-- replaced with `subB global5`.
				let outerTerm' = applySubstsBRP (M.singleton outerBindingName' subst) outerTerm

				(finishedOuterTerm, remainingOuterBindings') <-
					processOuterBindings (outerTerm', remainingOuterBindings)

				let finishedOuterBindings = (outerBindingName', outerBinding') : remainingOuterBindings'

				return (finishedOuterTerm, finishedOuterBindings)
		
			Just (originalInnerTerm, originalInnerBindings) ->
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
										subst = makeSubstBRP brp
											(length innerBindingParams)
											(freeAndGlobalVarsOfTermBRP brp valueOfTheParam)
											(const valueOfTheParam)
										in applySubstsBRP (M.singleton innerTermName subst) innerTerm

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
											&& n `S.notMember` S.unions (map (allParamNamesBRP brp) rest)
											| name <- S.toList (allParamNamesBRP brp param)]
										| param:rest <- tails outerBindingParams]

								let outerBindingParamsKept = filterByFlags outerBindingParamsKeepFlags outerBindingParams
								let newOuterBindingParams = outerBindingParamsKept ++ innerBindingParams

								let newOuterBinding = makeBindingBRP brp (newOuterBindingParams, outerBindingValue)

								let
									-- innerTermFun' :: [termType] -> termType
									innerTermFun' outerBindingParamValues = let
										innerTerm = innerTermFun outerBindingParamValues
										prunedParamValues = filterByFlags outerBindingParamsKeepFlags outerBindingParamValues
										bindingType = typeOfSubstInTermBRP innerTerm innerBindingName innerBinding
										subst = makeSubstBRP brp
											(length innerBindingParams)
											(S.singleton newOuterBindingName
												`S.union` freeAndGlobalVarsOfTypeBRP brp bindingType
												`S.union` S.unions (map (freeAndGlobalVarsOfTermBRP brp) prunedParamValues))
											(\ innerBindingParamValues -> makeTermInvokingSubstBRP brp
												newOuterBindingName
												bindingType
												(prunedParamValues ++ innerBindingParamValues)
												)
										in applySubstsBRP (M.singleton innerTermName subst) innerTerm

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
					processInnerBindings originalInnerBindings (const originalInnerTerm)

				let newVarsIntroduced =
						(freeAndGlobalVarsOfTermBRP brp originalInnerTerm
							S.\\ S.fromList (M.keys originalInnerBindings))
						`S.union` S.fromList (M.keys finalOuterBindings)
				let subst = makeSubstBRP brp
						(length outerBindingParams)
						newVarsIntroduced
						innerTermFun

				let outerTerm' = applySubstsBRP (M.singleton outerBindingName subst) outerTerm

				(finishedOuterTerm, remainingOuterBindings') <-
					processOuterBindings (outerTerm', remainingOuterBindings)

				let finishedOuterBindings = newOuterBindings ++ remainingOuterBindings'

				return (finishedOuterTerm, finishedOuterBindings)

	-- In our example, `finalOuterTerm` would be:
	--     globalFun (add . global1 (y . global2)) (add . global3 (y . global4)) (subB . global5 global6)
	-- and `finalOuterBindings` would have two bindings, named `subB` and `y`.

	-- finalOuterTerm :: termType
	-- finalOuterBindings :: [(nameType, bindingType)]
	(finalOuterTerm, finalOuterBindings) =
		flip evalState globals $
			processOuterBindings (originalOuterTerm, M.toList reducedOuterBindings)

	in (finalOuterTerm, M.fromList finalOuterBindings)


