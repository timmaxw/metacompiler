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

	generateUniqueNameBRP = (\ name forbidden -> let
		candidates = [NameOfSLType (unNameOfSLType name ++ replicate i '\'') | i <- [0..]]
		Just name' = find (`S.notMember` forbidden) candidates
		in name'),

	globalsOfLiteralBRP = SL.globalsInSLType,

	makeBindingBRP = (\ ([], value) -> SLTypeBinding value),
	unmakeBindingBRP = (\ (SLTypeBinding value) -> ([], value)),

	isMetaObjectALiteralBRP = (\ obj -> case obj of
		MOSLType lit bindings -> Just (lit, bindings)
		_ -> Nothing
		),

	paramNamesBRP = undefined,

	makeSubstBRP = (\ 0 _ subsFun -> TypeSub (\ [] -> subsFun [])),
	applySubstsBRP = (\ substs type_ -> runIdentity (substituteSLType substs type_)),
	makeLiteralInvokingSubstBRP = (\ name kind [] -> SLTypeName name kind)
	}

data BindingReductionParameters nameType literalType typeType bindingType paramType substType = BRP {
	generateUniqueNameBRP :: nameType -> S.Set nameType -> nameType,

	makeBindingBRP :: ([paramType], MetaObject) -> bindingType,
	unmakeBindingBRP :: bindingType -> ([paramType], MetaObject),

	isMetaObjectALiteralBRP :: MetaObject -> Maybe (literalType, M.Map nameType bindingType),

	paramNamesBRP :: paramType -> S.Set NameOfMetaObject,
	reduceParamBRP :: paramType -> paramType,
	
	makeSubstBRP :: Int -> S.Set nameType -> ([literalType] -> literalType) -> substType,
	applySubstsBRP :: M.Map nameType substType -> literalType -> literalType,
	typeOfSubstInLiteralBRP :: literalType -> nameType -> bindingType -> typeType,
	freeVarsAndGlobalsOfLiteralBRP :: literalType -> S.Set nameType,
	freeVarsAndGlobalsOfTypeBRP :: typeType -> S.Set nameType,
	makeLiteralInvokingSubstBRP :: nameType -> typeType -> [literalType] -> literalType
	}

{- `reduceBindings` is used for simplifying all types of literal expressions: SL terms and types, and JavaScript
expressions.

`reduceBindings` is very complicated. To explain how it works more clearly, I will refer back to the following example
in the comments in `reduceBindings`:

	(sl-term
		"globalFun (subX global1) (subX (subY global2 global3))"
		(term "subX" (param :: sl-term (sl-type "Nat")) =
			(sl-term "Succ x" (term "x" = param))
			)
		(term "subY" (param1 :: sl-term (sl-type "Nat")) (param2 :: sl-term (sl-type "Nat")) =
			someMetaObjectFunction param1
			)
		(term "subZ" (param :: sl-term (sl-type "Nat")) =
			someOtherMetaObjectFunction param
			)
		)

This ought to be reduced to:

	(sl-term
		"globalFun (Succ global1) (Succ (subY global2))"
		(term "subY" (param1 :: sl-term (sl-type "Nat")) =
			someMetaObjectFunction param1
			)
		)

-}

reduceBindings :: Ord nameType
               => BindingReductionParameters nameType literalType typeType bindingType paramType substType
               -> (literalType, M.Map nameType bindingType)
               -> (literalType, M.Map nameType bindingType)
reduceBindings brp (originalOuterLiteral, originalOuterBindings) = let

	-- `originalOuterLiteral` is the literal of the binding meta-object. In our example, `originalOuterLiteral` would
	-- be `globalFun (subX global1) (subX (subY global2 global3))`.

	-- `originalOuterBindings` is the bindings that come with 

	-- `filterByFlags` takes a list of objects and a list of `Bool`s saying whether to keep each one.
	filterByFlags :: [Bool] -> [a] -> [a]
	filterByFlags [] [] = []
	filterByFlags (True:flags) (a:objs) = a : filterByFlags flags objs
	filterByFlags (False:flags) (_:objs) = filterByFlags flags objs

	-- `reducedOuterBindings` is `originalOuterBindings` but with basic reductions performed on each binding.
	reducedOuterBindings = M.map
		(\ binding -> let
			(params, value) = makeBindingBRP brp binding
			params' = map (reduceParamBRP brp) params
			value' = reduceMetaObject value
			in makeBindingBRP brp (params', value'))
		originalOuterBindings

	-- `globals` is all of the global variables that are mentioned in both the original outer literal and the
	globals = (freeVarsAndGlobalsOfLiteralBRP brp originalOuterLiteral S.\\ S.fromList (M.keys reducedOuterBindings))
		`S.union` S.unions [
			case isMetaObjectALiteralBRP brp value of
				Nothing -> S.empty
				Just (innerLiteral, innerBindings) ->
					freeVarsAndGlobalsOfLiteralBRP brp innerLiteral S.\\ S.fromList (M.keys innerBindings)
			| (_, value) <- M.toList reducedOuterBindings]

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name.
	-- uniqify :: nameType -> State (S.Set nameType) nameType
	uniqify name = do
		takenNames <- get
		let name' = generateUniqueNameBRP brp name takenNames
		put (S.insert name' takenNames)
		return name'

	-- processOuterBindings :: (literalType, [(nameType, bindingType)])
	--                      -> State (S.Set nameType) (literalType, [(nameType, bindingType)])

	processOuterBindings (outerLiteral, []) = do
		return (outerLiteral, [])

	processOuterBindings (outerLiteral, ((outerBindingName, _):remainingOuterBindings)
		| outerBindingName `S.notMember` freeVarsAndGlobalsOfLiteralBRP brp outerLiteral = do
			-- This binding isn't used at all. We should just ignore it.
			processOuterBindings (outerLiteral, remainingOuterBindings)

	processOuterBindings (outerLiteral, outerBinding:remainingOuterBindings) = let
		(outerBindingParams, outerBindingValue) = unmakeBindingBRP brp outerBinding
		in case isMetaObjectALiteralBRP brp outerBindingValue of
			Nothing -> do
				-- This binding is used, but cannot be reduced further, so we must keep it. However, we might still have to
				-- rename it so it doesn't conflict with global names, and we might want to prune unused parameters.

				outerBindingName' <- uniqify outerBindingName

				-- It might be that the binding doesn't use all of its parameters. This list has `True` for every one
				-- that is used and `False` for every one that is no longer needed.
				let
					outerBindingParamsKeepFlags :: [Bool]
					outerBindingParamsKeepFlags = [
						or [
							-- This condition tests that the parameter's name appears in the expression.
							name `M.member` freeVarsInMetaObject outerBindingValue
							-- This condition tests that the parameter isn't being shadowed by another parameter.
							&& n `S.notMember` S.unions (map (paramNamesBRP brp) rest)
							| name <- S.toList (paramNamesBRP brp param)]
						| param:rest <- tails outerBindingParams]

				let outerBindingParams' = filterByFlags outerBindingParamsKeepFlags outerBindingParams

				let outerBinding' = makeBindingBRP brp (outerBindingParams', outerBindingValue)

				let bindingType = typeOfSubstInLiteralBRP outerLiteral outerBindingName outerBinding
				let subst = makeSubstBRP brp
					(length outerBindingParams)
					(S.singleton outerBindingName' `S.union` freeVarsAndGlobalsInTypeBRP brp bindingType)
					(\ outerBindingParamsValues -> makeLiteralInvokingSubstBRP brp
						outerBindingName'
						bindingType
						(filterByFlags outerBindingParamsKeepFlags outerBindingParamsValues))

				let outerLiteral' = applySubstsBRP (M.singleton outerBindingName' subst) outerLiteral

				(finishedOuterLiteral, remainingOuterBindings') <-
					processOuterBindings (outerLiteral', remainingOuterBindings)

				let finishedOuterBindings = (outerBindingName', outerBinding') : remainingOuterBindings'

				return (finishedOuterLiteral, finishedOuterBindings)
		
			Just (originalInnerLiteral, originalInnerBindings) ->

				let 
					-- processInnerBindings :: [(nameType, bindingType)]
					--                      -> ([literalType] -> literalType)
					--                      -> State (S.Set nameType) (
					--                             [literalType] -> literalType,
					--                             [(nameType, bindingType)]
					--                             )

					processInnerBindings [] innerLiteralFun = do
						return (innerLiteralFun, [])

					processInnerBindings ((innerBindingName, innerBinding):remainingInnerBindings) innerLiteralFun = do

						let (innerBindingParams, innerBindingValue) = unmakeBindingBRP brp innerBinding

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
									&& n `S.notMember` S.unions (map (paramNamesBRP brp) rest)
									| name <- S.toList (paramNamesBRP brp param)]
								| param:rest <- tails outerBindingParams]

						let outerBindingParamsKept = filterByFlags outerBindingParamsKeepFlags outerBindingParams
						let newOuterBindingParams = outerBindingParamsKept ++ innerBindingParams

						let newOuterBinding = makeBindingBRP brp (newOuterBindingParams, outerBindingValue)

						let
							-- innerLiteralFun' :: [literalType] -> literalType
							innerLiteralFun' outerBindingParamValues = let
								innerLiteral = innerLiteralFun outerBindingParamValues
								prunedParamValues = filterByFlags outerBindingParamsKeepFlags outerBindingParamValues
								bindingType = typeOfSubstInLiteralBRP innerLiteral innerBindingName innerBinding
								subst = makeSubstBRP brp
									(length innerBindingParams)
									(S.singleton newOuterBindingName
										`S.union` freeAndGlobalVarsOfTypeBRP brp bindingType
										`S.union` S.unions (map (freeAndGlobalVarsOfLiteralBRP brp) prunedParamValues))
									(\ innerBindingParamValues -> makeLiteralInvokingSubstBRP brp
										newOuterBindingName
										bindingType
										(prunedParamValues ++ innerBindingParamValues)
										)
								in applySubstsBRP (M.singleton innerLiteralName subst) innerLiteral

						(finalInnerLiteralFun, outerBindingsFromRemainingInnerBindings) <-
							processInnerBindings remainingInnerBindings innerLiteralFun'

						let finalOuterBindings =
								(newOuterBindingName, newOuterBinding) : outerBindingsFromRemainingInnerBindings

						return (finalInnerLiteralFun, finalOuterBindings)

				(innerLiteralFun, newOuterBindings) <-
					processInnerBindings originalInnerBindings (const originalInnerLiteral)

				let newVarsIntroduced =
						(freeAndGlobalVarsOfLiteralBRP brp originalInnerLiteral
							S.\\ S.fromList (M.keys originalInnerBindings))
						`S.union` S.fromList (M.keys finalOuterBindings)
				let subst = makeSubstBRP brp
						(length outerBindingParams)
						newVarsIntroduced
						innerLiteralFun
				let outerLiteral' = applySubstsBRP (M.singleton outerBindingName subst) outerLiteral

				(finishedOuterLiteral, remainingOuterBindings') <-
					processOuterBindings (outerLiteral', remainingOuterBindings)

				let finishedOuterBindings = newOuterBindings ++ remainingOuterBindings'

				return (finishedOuterLiteral, finishedOuterBindings)

	(finalOuterLiteral, finalOuterBindings) =
		flip evalState globals $
			processOuterBindings (originalOuterLiteral, M.toList reducedOuterBindings)

	in (finalOuterLiteral, M.fromList finalOuterBindings)


