module Metacompiler.SLRuntime.Match where

matchType :: (M.Map NameOfType Kind, M.Map NameOfType Kind)
          -> Type
          -> Type
          -> Maybe (M.Map NameOfType Kind, M.Map NameOfType Type, M.Map NameOfType Type)
matchType (wildcardTypes1, wildcardTypes2) type1 type2 = let
	takenTypeNames = (freeAndGlobalVarsInType type1 S.\\ M.keysSet wildcardTypes1)
			`S.union` (freeAndGlobalVarsInType type2 S.\\ M.keysSet wildcardTypes2)
	(renamedWildcardTypes1, renamedWildcardTypes2) = flip evalState takenTypeNames $ do
		rwts1 <- liftM M.fromList $ sequence [do
			name' <- uniqifyTypeName name
			return (name, name')
			| (name, _) <- M.toList wildcardTypes1]
		rwts2 <- liftM M.fromList $ sequence [do
			name' <- uniqifyTypeName name
			return (name, name')
			| (name, _) <- M.toList wildcardTypes2]
		return (rwts1, rwts2)
	initialState = UnifyState {
		typeFloatsUSt =
			M.fromList [(renamedWildcardTypes1 M.! name, FreeTypeFloat kind)
			           | (name, kind) <- M.toList wildcardTypes1]
			`M.union` [(renamedWildcardTypes2 M.! name, FreeTypeFloat kind)
			          | (name, kind) <- M.toList wildcardTypes],
		termFloatsUSt = M.empty
		}
	subbedType1 = runIdentity $ substituteType
		(M.fromList [let newType = TypeName (renamedWildcardTypes1 M.! name) kind
		            in (name, TypeSubst (runIdentity . foldr TypeApp newType))
		            | (name, kind) <- M.toList renamedWildcardTypes1])
		type1
	subbedType2 = runIdentity $ substituteType
		(M.fromList [let newType = TypeName (renamedWildcardTypes2 M.! name) kind
		            in (name, TypeSubst (runIdentity . foldr TypeApp newType))
		            | (name, kind) <- M.toList renamedWildcardTypes2])
		type2
	operation = do

performMatch :: (M.Map NameOfType Kind, M.Map NameOfType Kind)
             -> (M.Map NameOfTerm Type, M.Map NameOfTerm Type)
             -> S.Set NameOfType
             -> S.Set NameOfTerm
             -> ((Type -> Type) -> (Type -> Type) -> (Term -> Term) -> (Term -> Term) -> UnifyScope -> UnifyMonad ())
             -> Maybe (M.Map NameOfType Kind, M.Map NameOfType Type, M.Map NameOfType Type,
                       M.Map NameOfTerm Type, M.Map NameOfTerm Term, M.Map NameOfTerm Term)
performMatch (wildTypes1, wildTypes2) (wildTerms1, wildTerms2) takenTypeNames takenTermNames unifyFun = let
	(wildType1Names, wildType2Names) = flip evalState takenTypeNames $ do
		wt1ns <- mapM uniqifyTypeName (M.fromSet id (M.keySet wildTypes1))
		wt2ns <- mapM uniqifyTypeName (M.fromSet id (M.keySet wildTypes2))
		return (wt1ns, wt2ns)
	typeSubs1 = M.fromList [let newType = TypeName (wildType1Names M.! name) kind
	                        in (name, TypeSubst (runIdentity . foldr TypeApp newType))
	                       | (name, kind) <- M.toList wildTypes1]
	typeSubs2 = M.fromList [let newType = TypeName (wildType2Names M.! name) kind
	                        in (name, TypeSubst (runIdentity . foldr TypeApp newType))
	                       | (name, kind) <- M.toList wildTypes2]
	wildTerms1' = M.map (runIdentity . substituteTerm (typeSubs1, M.empty)) wildTerms1
	wildTerms2' = M.map (runIdentity . substituteTerm (typeSubs2, M.empty)) wildTerms2
	(wildTerm1Names, wildTerm2Names) = flip evalState takenTermNames $ do
		wt1ns <- mapM uniqifyTermName (M.fromSet id (M.keySet wildTerms1'))
		wt2ns <- mapM uniqifyTermName (M.fromSet id (M.keySet wildTerms2'))
		return (wt1ns, wt2ns)
	termSubs1 = M.fromList [let newName = wildTerm1Names M.! name
	                            newTerm = TermName newName type_
	                        in (name, TermSubst (S.singleton newName) (runIdentity . foldr TermApp newTerm))
	                       | (name, type_) <- M.toList wildTerms1']
	termSubs2 = M.fromList [let newName = wildTerm2Names M.! name
	                            newType = TypeName (wildTerm2Names M.! name) type_
	                        in (name, TermSubst (S.singleton newName) (runIdentity . foldr TermApp newTerm))
	                       | (name, type_) <- M.toList wildTerms2]
	scope = UnifyScope {
		typeFloatsInScope1USc = S.fromList (M.elems wildType1Names),
		typeFloatsInScope2USc = S.fromList (M.elems wildType2Names),
		termFloatsInScope1USc = S.fromList (M.elems wildTerm1Names),
		termFloatsInScope2USc = S.fromList (M.elems wildTerm2Names)
		}
	initialState = UnifyState {
		typeFloatsUSt = M.fromList $
			[(wildType1Names M.! name, FreeTypeFloat kind) | (name, kind) <- M.toList wildTypes1] ++
			[(wildType2Names M.! name, FreeTypeFloat kind) | (name, kind) <- M.toList wildTypes2],
		termFloatsUSt = M.fromList $
			[(wildTerm1Names M.! name, FreeTermFloat type_) | (name, type_) <- M.toList wildTerms1] ++
			[(wildTerm2Names M.! name, FreeTermFloat type_) | (name, type_) <- M.toList wildTerms2]
		}
	maybeFinalState = execStateT
		(unifyFun
			(substituteType typeSubs1)
			(substituteType typeSubs2)
			(substituteTerm (typeSubs1, termSubs1))
			(substituteTerm (typeSubs2, termSubs2))
			scope)
		initialState
	in case maybeFinalState of
		Nothing -> Nothing
		Just finalState -> Just (
			M.fromList [(name, kind) | (name, FreeTypeFloat kind) <- typeFloatsUSt finalState],
			M.fromList [
				let type_ = case typeFloatsUSt finalState M.! name' of
					FreeTypeFloat kind -> TypeName name' kind
					BoundTypeFloat t -> t
				in (name, t)
				| (name, name') <- M.toList wildTypeNames1],
			M.fromList [
				let type_ = case typeFloatsUSt finalState M.! name' of
					FreeTypeFloat kind -> TypeName name' kind
					BoundTypeFloat t -> t
				in (name, t)
				| (name, name') <- M.toList wildTypeNames2],
			M.fromList [(name, type_) | (name, FreeTermFloat type_) <- termFloatsUSt finalState],
			M.fromList [
				let type_ = case termFloatsUSt finalState M.! name' of
					FreeTermFloat type_ -> TermName name' type_
					BoundTermFloat t -> t
				in (name, t)
				| (name, name') <- M.toList wildTermNames1],
			M.fromList [
				let type_ = case termFloatsUSt finalState M.! name' of
					FreeTermFloat type_ -> TermName name' type_
					BoundTermFloat t -> t
				in (name, t)
				| (name, name') <- M.toList wildTermNames2]
			)

uniqifyTypeName :: TypeName -> State (S.Set TypeName) TypeName
uniqifyTypeName suggested = do
	taken <- get
	let candidates = [TypeName (unTypeName suggested ++ replicate i '\'') | i <- [0..]]
	let Just name = find (`S.notMember` suggested) candidates
	return name

uniqifyTermName :: TermName -> State (S.Set TermName) TermName
uniqifyTermName suggested = do
	taken <- get
	let candidates = [TermName (unTermName suggested ++ replicate i '\'') | i <- [0..]]
	let Just name = find (`S.notMember` suggested) candidates
	return name

type UnifyMonad = StateT UnifyState Maybe
data UnifyTypeFloat = FreeTypeFloat Kind | BoundTypeFloat Type
data UnifyTermFloat = FreeTermFloat Type | BoundTermFloat Term
data UnifyState = UnifyState {
	typeFloatsUSt :: M.Map NameOfType TypeFloat,
	termFloatsUSt :: M.Map NameOfTerm TermFloat
	}
data UnifyScope = UnifyScope {
	typeFloatsInScope1USc :: S.Set NameOfType,
	typeFloatsInScope2USc :: S.Set NameOfType,
	termFloatsInScope1USc :: S.Set NameOfTerm,
	termFloatsInScope2USc :: S.Set NameOfTerm
	}

unifyType :: UnifyScope -> Type -> Type -> MatchMonad ()
unifyType scope (TypeName n1 k2) (TypeName n2 k2)
	| n1 == n2 && n1 `M.member` typeFloatsInScope1USc && n2 `M.member` typeFloatsInScope2USC =
		return ()
unifyType scope (TypeName n1 k1) type2
	| n1 `M.member` typeFloatsInScope1USc scope = do
		state <- get
		case typeFloatsUSt state M.! n1 of
			FreeTypeFloat k -> do
				-- Detect loops
				when (n1 `M.member` freeVarsInType type2 && n1 `M.member` typeFloatsInScope2USc scope) Nothing
				type2' <- expandTypeFloats scope type2
				put (state { typeFloatsUSt = M.insert n1 (BoundTypeFloat type2') (typeFloatsUSt state) })
			BoundTypeFloat type1 -> unifyType scope type1 type2
unifyType scope type1 (TypeName n2 k2)
	| n2 `M.member` typeFloatsInScope2USc scope = do
		state <- get
		case typeFloatsUSt state M.! n2 of
			FreeTypeFloat k -> do
				-- Detect loops
				when (n2 `M.member` freeVarsInType type1 && n2 `M.member` typeFloatsInScope1USc scope) Nothing
				type1' <- expandTypeFloats type1
				put (state { typeFloatsUSt = M.insert n2 (BoundTypeFloat type1') (typeFloatsUSt state) })
			BoundTypeFloat type2 -> unifyType scope type1 type2
unifyType scope (TypeDefined d1) (TypeDefined d2) =
	unless (nameOfDataDefn d1 == nameOfDataDefn d2) Nothing
unifyType scope (TypeName n1 k1) (TypeName n2 k2) =
	unless (n1 == n2) Nothing
unifyType scope (TypeApp f1 x1) (TypeApp f2 x2) = do
	unifyType f1 f2
	unifyType x1 x2
unifyType scope (TypeFun a1 r1) (TypeFun a2 r2) = do
	unifyType a1 a2
	unifyType r1 r2
unifyType scope (TypeLazy x1) (TypeLazy s2) = do
	unifyType x1 x2

expandTypeFloats :: Type -> MatchMonad Type
expandTypeFloats type_ = do
	state <- get
	let subs = M.fromList [(n, TypeSubst (Identity . foldl TypeApp v))
	                      | (n, BoundTypeFloat v) <- M.toList (typeFloatsUSt state)]
	let type_' = runIdentity (substituteType subs type_)
	return type_'
