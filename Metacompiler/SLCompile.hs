module SLCompile where

data TypeInScope = TypeInScope {
	typeParamsOfTypeInScope :: [R.SLKind],
	valueOfTypeInScope :: [R.MetaObject] -> R.MetaObject
	}

data TermInScope = TermInScope {
	typeParamsOfTermInScope :: [R.SLKind],
	termParamsOfTermInScope :: [R.MetaObject],
	valueOfTermInScope :: [R.MetaObject] -> [R.MetaObject] -> R.MetaObject
	}

data CtorInScope = CtorInScope {
	typeParamsOfCtorInScope :: [R.SLKind],

compileSLKind :: SLS.Kind Range -> Either String R.SLKind
...

compileSLType :: M.Map SLS.NameOfType TypeInScope
              -> SLS.Type Range
              -> [R.MetaObject]
              -> Either String R.MetaObject
compileSLType typeScope typeParams (SLS.TypeName range name) =
	case M.lookup name typeScope of
		Just typeInScope -> do
			when (length typeParams < length (typeParamsOfTypeInScope typeInScope)) $
				Left ("at " ++ formatRange range ++ ": substitution `" ++ SLS.unNameOfType name ++ "` takes " ++
					show (length (typeParamsOfTypeInScope typeInScope)) ++ " parameters, but only " ++
					show (length typeParams) ++ " were provided.")
			let typeParamsToSub = take (length (typeParamsOfTypeInScope typeInScope)) typeParams
			let typeParamsNotToSub = drop (length (typeParamsOfTypeInScope typeInScope)) typeParams
			sequence [do
				let actualKind = slKindOfMetaObject type_
				unless (actualKind == expectedKind) $
					Left ("at " ++ formatRange range ++ ": parameter #" ++ show i ++ " to substitution `" ++
						SLS.unNameOfType name ++ "` should have kind " ++ formatKind expectedKind ++ ", but actually \
						\has kind " ++ formatKind actualKind)
				| (type_, expectedKind, i) <- zip3 typeParamsToSub (typeParamsOfTypeInScope typeInScope) [1..]]
			value <- valueOfTypeInScope typeInScope typeParamsToSub
			compileSLTypeApps value typeParamsNotToSub
		Nothing -> Left ("at " ++ formatRange range ++ ": type name `" ++ SLS.unNameOfType name ++ "` is not in scope")
compileSLType typeScope typeParams (SLS.TypeApp range fun arg) = do
	arg' <- compileSLType typeScope [] arg
	compileSLType typeScope (arg':typeParams) fun
compileSLType typeScope typeParams other | not (null typeParams) = do
	other' <- compileSLType typeScope [] other
	compileSLTypeApps other' typeParams
...

compileSLTypeApps :: R.MetaObject -> [R.MetaObject] -> Either String R.MetaObject
...

compileSLTerm :: M.Map SLS.NameOfType TypeInScope
              -> M.Map SLS.NameOfTerm TermInScope
              -> SLS.Term Range
              -> [R.MetaObject]
              -> Either String R.MetaObject
...

