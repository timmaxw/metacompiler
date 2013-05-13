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

	freeVarsAndGlobalsOfLiteralBRP :: literalType -> S.Set nameType,

	makeBindingBRP :: ([paramType], MetaObject) -> bindingType,
	unmakeBindingBRP :: bindingType -> ([paramType], MetaObject),

	isMetaObjectALiteralBRP :: MetaObject -> Maybe (literalType, M.Map nameType bindingType),

	paramNamesBRP :: paramType -> S.Set NameOfMetaObject,
	reduceParamBRP :: paramType -> paramType,
	
	makeSubstBRP :: Int -> S.Set nameType -> ([literalType] -> literalType) -> substType,
	applySubstsBRP :: M.Map nameType substType -> literalType -> literalType,

	typeOfSubstInLiteralBRP :: (literalType, M.Map nameType bindingType) -> nameType -> typeType,
	makeLiteralInvokingSubstBRP :: nameType -> typeType -> [literalType] -> literalType
	}

reduceBindings :: Ord nameType
               => BindingReductionParameters nameType literalType typeType bindingType paramType substType
               -> (literalType, M.Map nameType bindingType)
               -> (literalType, M.Map nameType bindingType)
reduceBindings brp (outerLiteral, outerBindings) = let

	-- `filterByFlags` takes a list of objects and a list of bools saying whether to keep each one.
	filterByFlags :: [Bool] -> [a] -> [a]
	filterByFlags [] [] = []
	filterByFlags (True:flags) (a:objs) = a : filterByFlags flags objs
	filterByFlags (False:flags) (_:objs) = filterByFlags flags objs

	-- `outerBindings'` is `outerBindings` but with basic reductions performed on each binding.
	outerBindings' = M.map
		(\ binding -> let
			(params, value) = makeBindingBRP brp binding
			params' = map (reduceParamBRP brp) params
			value' = reduceMetaObject value
			in makeBindingBRP brp (params', value'))
		outerBindings

	globals = (freeVarsAndGlobalsOfLiteralBRP brp outerLiteral S.\\ S.fromList (M.keys outerBindings'))
		`S.union` S.unions [
			case isMetaObjectALiteralBRP brp value of
				Nothing -> S.empty
				Just (innerLiteral, innerBindings) ->
					freeVarsAndGlobalsOfLiteralBRP brp innerLiteral S.\\ S.fromList (M.keys innerBindings)
			| (_, value) <- M.toList outerBindings']

	-- The `StateT` monad is used to keep track of which names are safe to use. `uniqify` changes a name if necessary
	-- to make it unique, then records the new name so nothing else uses it, and returns the new name.
	-- uniqify :: nameType -> State (S.Set nameType) nameType
	uniqify name = do
		takenNames <- get
		let name' = generateUniqueNameBRP brp name takenNames
		put (S.insert name' takenNames)
		return name'

	processBindings :: literalType -> 

	-- processedBindingsAndSubs :: [(
	--     M.Map nameType bindingType,
	--     M.Map nameType substType
	--     )]
	processedBindingsAndSubs = flip evalState globals $ sequence [
		case isMetaObjectALiteralBRP brp outerBindingValue of

			-- This binding isn't used at all. We can just throw it away.
			_ | outerBindingName `S.notMember` freeVarsAndGlobalsOfLiteralBRP brp outerLiteral ->
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
					(S.singleton outerBindingName')
					(\ outerBindingParamsValues -> makeLiteralInvokingSubstBRP brp
						outerBindingName'
						(typeOfSubstInLiteralBRP (outerLiteral, outerBindings') outerBindingName)   -- TODO: substitute within this
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

					let subsForInnerLiteral = (\ outerBindingParamValues -> let
						valuesToKeep = filterByFlags outerBindingParamsKeepFlags outerBindingParamValues
						namesIntroduced = S.singleton newOuterBindingName
							`S.union` S.unions (map (globalsInLiteralBRP brp) valuesToKeep)
							`S.union` S.unions (map (S.fromList . M.keys . freeNamesInLiteralBRP brp) valuesToKeep)
						in M.singleton innerBindingName $ makeSubstBRP brp
							(length newOuterBindingParams)
							namesIntroduced
							(\ innerBindingParamValues -> makeLiteralInvokingSubstBRP brp
								newOuterBindingName
								(typeOfSubstInLiteralBRP brp (innerLiteral, innerBindings) innerBindingName)
								(valuesToKeep ++ innerBindingParamValues)
								)
						)

					return (
						M.singleton newOuterBindingName (makeBindingBRP brp (newOuterBindingParams, innerBindingValue)),
						subsForInnerLiteral
						)

					| (innerBindingName, innerBinding) <- M.toList innerBindings,
					  let (innerBindingParams, innerBindingValue) = unmakeBindingBRP brp innerBinding]

				let subst = makeSubstBRP brp
						(length outerBindingParams)
						(S.fromList (M.keys newOuterBindings)
							`S.union` globalsOfLiteralBRP brp innerLiteral)
						(\ outerBindingParamsValues ->
							applySubstsBRP brp
								(subsForInnerLiteral outerBindingParamsValues)
								innerLiteral)

				return (
					newOuterBindings,
					M.singleton outerBindingName subst
					)

		| (outerBindingName, outerBinding) <- M.toList outerBindings',
		  let (outerBindingParams, outerBindingValue) = unmakeBindingBRP brp outerBinding]

	-- `processedBindings` is the newly reduced bindings. `subs` is the substitutions to apply to the outer object.
	(processedBindings, subs) = mconcat processedBindingsAndSubs

	in (processedBindings, subs)

