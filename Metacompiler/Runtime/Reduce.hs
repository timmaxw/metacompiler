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

reduceSLTypeBindings :: S.Set NameOfSLType
                     -> M.Map NameOfSLType SLKind
                     -> M.Map NameOfSLType SLTypeBinding
                     -> (M.Map NameOfSLType SLTypeBinding, M.Map NameOfSLType SL.TypeSub)
reduceSLTypeBindings globalsInOuterObject freeVarsInOuterObject bindings = let

	-- Recursively simplify the bindings
	bindings' = M.map (\ (SLTypeBinding v) ->
		SLTypeBinding
			(reduceMetaObject v)
		) bindings

	-- Collect a list of all the global variables referred to in either the outermost binding or any sub-bindings.
	-- These names should not be used for bindings, or else there will be name collisions.
	forbiddenNames = globalsInOuterObject
		`S.union` S.unions [globalsInSLType innerType
			| (_, SLTypeBinding (MOSLType innerType _)) <- M.toList bindings']

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name.
	uniqify :: NameOfSLType -> State (S.Set NameOfSLType) NameOfSLType
	uniqify name = do
		takenNames <- get
		let candidates = [SLTypeName (unSLTypeName n ++ replicate i '\'') | i <- [0..]]
		let Just name' = find (`S.notMember` takenNames) candidates
		put (S.insert name' takenNames)
		return name'

	-- processedBindingsAndSubs :: [(M.Map NameOfSLType SLTypeBinding, M.Map NameOfSLType SL.TypeSub)]
	processedBindingsAndSubs = flip evalState forbiddenNames $ sequence [do
		case value of

			-- If the binding isn't used at all, we can just throw it away
			_ | name `M.notMember` freeVarsInOuterObject ->
				return (M.empty, M.empty)

			-- If the binding is another thing of the same type, we can substitute it in
			MOSLType innerType innerBindings -> do
				-- All of the inner bindings will become outer bindings. But we might have to rename them so that they
				-- don't conflict with any forbidden names. `renamedInnerBindings` are the bindings to become outer
				-- bindings; `renamings` are substitutions to perform on `innerType` to compensate for the changes in
				-- the names of the bindings.
				(renamedInnerBindings, renamings) <- liftM mconcat $ sequence [do
					innerName' <- uniqify innerName
					let innerValueKind = freeVarsInSLType innerType M.! innerName
					return (
						M.singleton innerName' (SLTypeBinding innerValue),
						M.singleton innerName (simpleTypeSub (SLTypeName innerName' innerValueKind))
						)
					| (innerName, SLTypeBinding innerValue) <- M.toList innerBindings]
				let innerType' = substituteSLType renamings innerType
				return (renamedInnerBindings, M.singleton name (simpleTypeSub innerType'))

			-- Otherwise, leave the binding as a binding; but we might have to rename it so that it doesn't conflict
			-- with any forbidden names
			_ -> do
				name' <- uniqify name
				let valueKind = freeVarsInOuterObject M.! name
				return (
					M.singleton name' (SLTypeBinding value),
					if name == name' then M.empty else M.singleton name (simpleTypeSub (SLTypeName name' valueKind))
					)
		| (name, SLTypeBinding value) <- M.toList bindings']

	(processedBindings, subs) = mconcat processedBindings
	in (processedBindings, subs)

data BindingReductionParameters nameType literalType bindingType paramType substType = BRP {
	makeNameBRP :: String -> nameType,
	unmakeNameBRP :: nameType -> String,
	makeBindingBRP :: ([paramType], MetaObject) -> bindingType,
	unmakeBindingBRP :: bindingType -> ([paramType], MetaObject),
	isMetaObjectALiteralBRP :: MetaObject -> Maybe (literalType, M.Map nameType bindingType),
	paramNamesBRP :: paramType -> S.Set NameOfMetaObject,
	makeSubstBRP :: Int -> ? -> ([literalType] -> literalType) -> substType,
	applySubstsBRP :: M.Map nameType substType -> literalType -> literalType,
	makeLiteralInvokingSubstBRP :: nameType -> [literalType] -> literalType
	}

reduceBindings :: Ord nameType
               => BindingReductionParameters nameType literalType bindingType paramType substType
               -> S.Set nameType
               -> S.Set nameType
               -> M.Map nameType bindingType
               -> (M.Map nameType bindingType, M.Map nameType substType)
reduceBindings brp globalNames freeNamesInOuterLiteral bindings = let

	-- `filterByFlags` takes a list of objects and a list of bools saying whether to keep each one.
	filterByFlags :: [Bool] -> [a] -> [a]
	filterByFlags [] [] = []
	filterByFlags (True:flags) (a:objs) = a : filterByFlags flags objs
	filterByFlags (False:flags) (_:objs) = filterByFlags flags objs

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name.
	-- uniqify :: nameType -> State (S.Set nameType) nameType
	uniqify name = do
		takenNames <- get
		let candidates = [(makeNameBRP brp) ((unMakeNameBRP brp) n ++ replicate i '\'') | i <- [0..]]
		let Just name' = find (`S.notMember` takenNames) candidates
		put (S.insert name' takenNames)
		return name'

	-- processedBindingsAndSubs :: [(M.Map nameType bindingType, M.Map nameType substType)]
	processedBindingsAndSubs = flip evalState globalNames $ sequence [
		case isMetaObjectALiteralBRP brp outerBindingValue of

			-- This binding isn't used at all. We can just throw it away.
			_ | outerBindingName `S.notMember` freeNamesInOuterObject ->
				return (M.empty, M.empty)

			-- This binding is used, but even after being reduced it isn't a literal of the appropriate type. We can't
			-- fold it into the outer literal, but we might have to rename it, and we can still prune its parameters.
			Nothing -> do
				-- Rename the binding to avoid conflicts
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
							&& n `S.notMember` S.unions (map paramNamesBRP rest)
							| name <- S.toList (paramNamesBRP brp param)]
						| param:rest <- tails outerBindingParams]

				let outerBindingParams' = filterByFlags outerBindingParamsKeepFlags outerBindingParams
				
				let subst = makeSubstBRP brp
					(length outerBindingParams)
					(...)
					(\ outerBindingParamsValues -> makeLiteralInvokingSubstBRP brp
						outerBindingName'
						(filterByFlags outerBindingParamsKeepFlags outerBindingParamsValues))

				return (
					M.singleton outerBindingName' (makeBindingBRP outerBindingParams' outerBindingValue),
					M.singleton outerBindingName subs
					)

			-- This binding is used, and it reduces to a literal, so we can fold it into the outer literal.
			Just (innerLiteral, innerBindings) -> do

				-- newOuterBindings :: M.Map nameType bindingType
				-- subsForInnerLiteral :: [literalType] -> M.Map nameType substType
				(newOuterBindings, subsForInnerLiteral) <- liftM mconcat $ sequence [do
				
					newOuterBindingName <- uniqify innerBindingName

					-- It might be that not all of the outer binding parameters are used in this inner binding. This
					-- list has `True` for every one that is used and `False` for every one that is no longer needed.
					let
						outerBindingParamsKeepFlags :: [Bool]
						outerBindingParamsKeepFlags = [
							or [
								-- This condition tests that the parameter's name appears in the expression.
								name `M.member` freeVarsInMetaObject innerBindingValue
								-- This condition tests that the parameter isn't being shadowed by another parameter.
								&& n `S.notMember` S.unions (map paramNamesBRP rest)
								| name <- S.toList (paramNamesBRP brp param)]
							| param:rest <- tails outerBindingParams]

					let outerBindingParamsKept = filterByFlags outerBindingParamsKeepFlags outerBindingParams
					let newOuterBindingParams = outerBindingParamsKept ++ innerBindingParams

					let subsForInnerLiteral outerBindingParamValues =
							M.singleton innerBindingName $ makeSubstBRP brp
								(length newOuterBindingParams)
								(...)
								(\ innerBindingParamValues -> let
									newOuterBindingParamValues =
										filterByFlags outerBindingParamsKeepFlags outerBindingParamValues
										++ innerBindingParamValues
									in makeLiteralInvokingSubstBRP brp newOuterBindingName newOuterBindingParamValues
									)

					return (
						M.singleton newOuterBindingName (makeBindingBRP brp (newOuterBindingParams, innerBindingValue)),
						subsForInnerLiteral
						)

					| (innerBindingName, innerBinding) <- M.toList innerBindings,
					  let (innerBindingParams, innerBindingValue) = unmakeBindingBRP brp innerBinding]

				let subst = makeSubstBRP brp
						(length outerBindingParams)
						(...)
						(\ outerBindingParamsValues ->
							applySubstsBRP brp
								(subsForInnerLiteral outerBindingParamsValues)
								innerLiteral)

				return (
					newOuterBindings,
					M.singleton outerBindingName subst
					)

		| (outerBindingName, outerBinding) <- M.toList bindings,
		  let (outerBindingParams, outerBindingValue) = unmakeBindingBRP brp outerBinding]

	-- `processedBindings` is the newly reduced bindings. `subs` is the substitutions to apply to the outer object.
	(processedBindings, subs) = mconcat processedBindingsAndSubs

	in (processedBindings, subs)

reduceSLTermBindings :: S.Set NameOfSLTerm
                     -> M.Map NameOfSLTerm SLType
                     -> M.Map NameOfSLTerm SLTermBinding
                     -> (M.Map NameOfSLTerm SLTermBinding, M.Map NameOfSLType SLTypeBinding, M.Map NameOfSLTerm SL.TermSub)
reduceSLTermBindings globalsInOuterObject freeVarsInOuterObject bindings = let

	-- Recursively simplify the bindings
	bindings' = M.map (\ (SLTermBinding params value) ->
		SLTermBinding
			[(n, reduceMetaObject t) | (n, t) <- params]
			(reduceMetaObject value)
		) bindings

	-- Collect a list of all the global variables referred to in either the outermost binding or any sub-bindings.
	-- These names should not be used for bindings, or else there will be name collisions.
	forbiddenNames = globalsInOuterObject
		`S.union` S.unions [
			globalsInSLTerm innerTerm
			| (_, SLTermBinding _ (MOSLTerm innerTerm _)) <- M.toList bindings']

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name.
	uniqify :: NameOfSLTerm -> State (S.Set NameOfSLTerm) NameOfSLTerm
	uniqify name = do
		takenNames <- get
		let candidates = [SLTermName (unSLTermName n ++ replicate i '\'') | i <- [0..]]
		let Just name' = find (`S.notMember` takenNames) candidates
		put (S.insert name' takenNames)
		return name'

	-- processedBindingsAndSubs :: [(M.Map NameOfSLTerm SLTermBinding, M.Map NameOfSLTerm SL.TermSub)]
	processedBindingsAndSubs = flip evalState forbiddenNames $ sequence [
		case value of

			-- If the binding isn't used at all, we can just throw it away
			_ | name `M.notMember` freeVarsInOuterObject ->
				return (M.empty, M.empty)

			-- If the binding is another thing of the same type, we can substitute it in
			MOSLType innerType innerBindings -> do
				-- All of the inner bindings will become outer bindings. But we might have to rename them so that they
				-- don't conflict with any forbidden names, and the parameters might change. `renamedInnerBindings` are
				-- the bindings to become outer bindings; `renamings` are substitutions to perform on `innerType` to
				-- compensate for the changes in the names and parameters of the bindings.
				(renamedInnerBindings, renamings) <- liftM mconcat $ sequence [do
					innerName' <- uniqify innerName

					let
						-- The parameters from the outer binding will be "grafted" on to the parameter lists of the
						-- inner bindings. However, not all of the parameters will be necessary. `graftedParamsToKeep`
						-- contains a `True` for each element of `params` if that parameter is necessary for this
						-- binding, and a `False` otherwise.
						graftedParamsToKeep :: [Bool]
						graftedParamsToKeep = [
							-- This condition tests that the parameter's name appears in the expression.
							n `M.member` freeVarsInMetaObject innerValue
							-- This condition tests that the parameter isn't being shadowed by another parameter.
							&& n `notElem` (map fst rest)
							| (n, _):rest <- tails params]

					-- `graftedParams` is a subset of `params`, with unused parameters removed.
					let graftedParams = [p | (p, keep) <- zip params graftedParamsToKeep, keep]

					let innerValueKind = freeVarsInSLType innerType M.! innerName

					let subs = 

					return (
						M.singleton innerName' (SLTypeBinding innerValue),
						M.singleton innerName (simpleTypeSub (SLTypeName innerName' innerValueKind))
						)
					| (innerName, SLTermBinding innerParams innerValue) <- M.toList innerBindings]
				let innerType' = substituteSLType renamings innerType
				return (renamedInnerBindings, M.singleton name (simpleTypeSub innerType'))

			-- Otherwise, leave the binding as a binding. We might still have to rename the binding so it doesn't
			-- conflict, or to throw away unused parameters.
			_ -> do
				name' <- uniqify name

				let
					-- `paramsToKeep` contains a `True` for each parameter that should be kept and a `False` for each
					-- parameter that should be thrown away.
					paramsToKeep :: [Bool]
					paramsToKeep = [
						-- This condition tests that the parameter's name appears in the expression.
						n `M.member` freeVarsInMetaObject value
						-- This condition tests that the parameter isn't being shadowed by another parameter.
						&& n `notElem` (map fst rest)
						| (n, _):rest <- tails params]

				-- `params'` is a subset of `params`, with unused parameters removed.
				let params' = [p | (p, keep) <- zip params paramsToKeep, keep]

				let valueType = freeVarsInOuterObject M.! name

				let subs = M.singleton name (TermSub
						(S.singleton name')
						(\args -> foldl SLTermApp
							(SLTermName name' valueType)
							-- Alter the call site by throwing away arguments corresponding to no-longer-used
							-- parameters
							[a | (a, keep) <- zip args paramsToKeep, keep])
						)

				return (M.singleton name' (SLTermBinding params' value), subs)

		| (name, SLTermBinding params value) <- M.toList 

	(processedBindings, subs) = mconcat processedBindings
	in (processedBindings, subs)

