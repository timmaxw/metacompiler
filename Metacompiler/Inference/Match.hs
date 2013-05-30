module Metacompiler.Inference.Match where

data MetaObjectPattern = MetaObjectPattern {
	matchMetaObjectPattern :: MetaObject
	                       -> Maybe (M.Map NameOfMetaObject MetaObject)
	}

data SLTypePattern = SLTypePattern {
	matchSLTypePattern :: SLR.Type
	                   -> M.Map SLR.NameOfType SLR.TypeBinding
	                   -> Maybe (M.Map NameOfMetaObject MetaObject)
	}

data SLTermPattern = SLTermPattern {
	matchSLTypePattern :: SLR.Term
	                   -> M.Map SLR.NameOfType SLR.TypeBinding
	                   -> M.Map SLR.NameOfTerm SLR.TermBinding
	                   -> Maybe (M.Map NameOfMetaObject MetaObject)
	}

compileMetaObjectPattern :: M.Map NameOfMetaObject MetaType
                         -> MetaObject
                         -> ErrorMonad MetaObjectPattern
compileMetaObjectPattern freeVars object = do
	let

	pattern <- scanMetaObject freeVars (reduceMetaObject object)
	let pattern' = MetaObjectPattern (\obj -> matchMetaObjectPattern pattern (reduceMetaObject obj))
	return pattern'

scanMetaObject :: M.Map NameOfMetaObject MetaType
               -> MetaObject
               -> ErrorMonad MetaObjectPattern
scanMetaObject freeVars (MOApp fun (MOName name type_)) | name `M.notMember` freeVars = do
	funPattern <- scanMetaObject freeVars fun
	return $ MetaObjectPattern (\obj -> matchMetaObjectPattern funPattern (MOAbs (name, type_) obj))
scanMetaObject freeVars (MOApp fun _) =
	fail ("functions should be applied only to names in scope")
scanMetaObject freeVars (MOAbs (name, type_) body) = do
	bodyPattern <- scanMetaObject (freeVars `M.delete` name) body
	return $ MetaObjectPattern (\obj -> case obj of
		MOAbs (name', _) body' ->
			matchMetaObjectPattern bodyPattern
				(substituteMetaObject (M.singleton name' (MOName name type_)) body')
		_ -> Nothing)
scanMetaObject freeVars (MOName name type_) | name `M.member` freeVars =
	return $ MetaObjectPattern (\obj ->
		if M.null (freeVarsInMetaObject obj)
			then Just (M.singleton name obj)
			else Nothing)
scanMetaObject freeVars (MOName name _) =
	return $ MetaObjectPattern (\obj -> case obj of
		MOName name' _ | name == name' -> Just M.empty
		_ -> Nothing)
scanMetaObject freeVars (MOSLType type_ typeBindings) = do
	typeBindingPatterns <- liftM M.fromList $ sequence [do
		valuePattern <- scanMetaObject freeVars value
		return (name, SLTypeBinding valuePattern)
		| (name, SLTypeBinding value) <- M.toList typeBindings]
	return $ MetaObjectPattern (\obj -> case obj of
		SLTypeBinding type_' typeBindings' ->
			matchSLType type_ typeBindingPatterns type_' typeBindings'
		_ -> Nothing)
scanMetaObject freeVars (MOSLTerm term typeBindings termBindings) = do
	typeBindingPatterns <- liftM M.fromList $ sequence [do
		valuePattern <- scanMetaObject freeVars value
		return (name, valuePattern)
		| (name, SLTypeBinding) <- M.toList typeBindings]
	termBindingPatterns <- liftM M.fromList $ sequence [do
		valuePattern <- scanMetaObject (foldr M.delete freeVars (map fst params)) value
		return (name, (params, valuePattern))
		| (name, SLTermBinding params value) <- M.toList termBindings]
	return $ MetaObjectPattern (\obj -> case obj of
		SLTermBinding term' typeBindings' termBindings' ->
			matchSLTerm term typeBindingPatterns termBindingPatterms term' typeBindings' termBindings'
		_ -> Nothing)
scanMetaObject freeVars (MOJSExprTypeDefn defn params) = do
	paramPatterns <- mapM (scanMetaObject freeVars params)
	return $ MetaObjectPattern (\obj -> case obj of
		MOJSExprTypeDefn defn' params' | nameOfJSExprTypeDefn defn == nameOfJSExprTypeDefn defn' -> do
			matches <- sequence [matchMetaObjectPattern pattern value
				| (pattern, value) <- zip paramPatterns params']
			foldM combineMatches M.empty matches
		_ -> Nothing)
scanMetaObject freeVars _ = fail ("can't match that type of pattern")

scanSLType :: M.Map SLR.NameOfType MetaObjectPattern
           -> SLR.Type
           -> ErrorMonad SLTypePattern
scanSLType _ (SLR.TypeDefined defn) =
	return $ SLTypePattern (\ type_ _ -> case type_ of
		SLR.TypeDefined defn' | defn == defn' -> return M.empty
		_ -> Nothing)
scanSLType typePatterns (SLR.TypeName name _) | name `M.member` typePatterns =
	return $ SLTypePattern (\ type_ typeBindings ->
		matchMetaObjectPattern pattern (MOSLType type_ typeBindings)
		)
	where pattern = typePatterns M.! name
scanSLType typePatterns (SLR.TypeApp fun arg) = do
	funPattern <- scanSLType typePatterns fun
	argPattern <- scanSLType typePatterns arg
	

matchSLType :: SLR.Type -> M.Map SLR.NameOfType MetaObjectPattern
            -> SLR.Type -> M.Map SLR.NameOfType SLTypeBinding
            -> Maybe (M.Map NameOfMetaObject MetaObject)
matchSLType (SLR.TypeDefined defn1) _ (SLR.TypeDefined defn2) _ | defn1 == defn2 =
	return M.empty
matchSLType (SLR.TypeName name _) ptybs value vtybs | name1 `M.member` ptbs = do
	let pattern = ptybs M.! name
	let value' = reduceMetaObject (MOSLType value vtybs)
	matchMetaObjectPattern pattern value'
matchSLType (SLR.TypeApp fun1 arg1) ptybs (SLR.TypeApp fun2 arg2) vtybs = do
	funMatches <- matchSLType fun1 ptybs fun2 vtybs
	argMatches <- matchSLType arg1 ptybs arg2 vtybs
	combineMatches funMatches argMatches
matchSLType (SLR.TypeFun arg1 ret1) ptybs (SLR.TypeFun arg2 ret2) vtybs = do
	argMatches <- matchSLType arg1 ptybs arg2 vtybs
	retMatches <- matchSLType ret1 ptybs ret2 vtybs
	combineMatches argMatches retMatches
matchSLType (SLR.TypeLazy x1) ptybs (SLR.TypeLazy x2) vtybs =
	matchSLType x1 ptybs x2 vtybs
matchSLType _ _ _ _ = Nothing

matchSLTerm :: SLR.Term
            -> [SLR.Term]
            -> M.Map SLR.NameOfType MetaObjectPattern
            -> M.Map SLR.NameOfTerm ([(NameOfMetaObject, MetaType)], MetaObjectPattern)
            -> SLR.Term
            -> M.Map SLR.NameOfType SLTypeBinding
            -> M.Map SLR.NameOfTerm SLTermBinding
            -> Maybe (M.Map NameOfMetaObject MetaObject)
matchSLTerm (SLR.TermDefined defn1 params1) [] ptybs ptebs (SLR.TermDefined defn2 params2) vtybs vtebs
	| defn1 == defn2 = do
		paramMatches <- sequence [matchSLType type1 ptybs type2 vtybs | (type1, type2) <- zip params1 params2]
		foldM combineMatches M.empty paramMatches
matchSLTerm (SLR.TermName name _) patternArgs _ ptebs value valueArgs vtybs vtebs
	| name `M.member` ptebs && length patternArgs == length params = do
		let value' = reduceMetaObject (MOSLTerm value vtybs vtebs)
		
	where (params, pattern) = ptebs M.! name

