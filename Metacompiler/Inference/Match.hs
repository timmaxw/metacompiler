module Metacompiler.Inference.Match where

data MetaObjectPattern = MetaObjectPattern {
	matchMetaObjectPattern :: MetaObject -> Maybe (M.Map NameOfMetaObject MetaObject)
	}

compileMetaObjectPattern :: M.Map NameOfMetaObject MetaType
                         -> MetaObject
                         -> ErrorMonad MetaObjectPattern
compileMetaObjectPattern freeVars object = do
	let
		scan :: M.Map NameOfMetaObject MetaType
		     -> MetaObject
		     -> ErrorMonad MetaObjectPattern
		scan freeVars (MOApp fun (MOName name type_)) | name `M.notMember` freeVars = do
			funPattern <- scan freeVars fun
			return $ MetaObjectPattern (\obj -> matchMetaObjectPattern funPattern (MOAbs (name, type_) obj))
		scan freeVars (MOApp fun _) =
			fail ("functions should be applied only to names in scope")
		scan freeVars (MOAbs (name, type_) body) = do
			bodyPattern <- scan (freeVars `M.delete` name) body
			return $ MetaObjectPattern (\obj -> case obj of
				MOAbs (name', _) body' ->
					matchMetaObjectPattern bodyPattern
						(substituteMetaObject (M.singleton name' (MOName name type_)) body')
				_ -> Nothing)
		scan freeVars (MOName name type_) | name `M.member` freeVars =
			return $ MetaObjectPattern (\obj ->
				if M.null (freeVarsInMetaObject obj)
					then Just (M.singleton name obj)
					else Nothing)
		scan freeVars (MOName name _) =
			return $ MetaObjectPattern (\obj -> case obj of
				MOName name' _ | name == name' -> Just M.empty
				_ -> Nothing)
		scan freeVars (MOSLType type_ typeBindings) = do
			typeBindingPatterns <- liftM M.fromList $ sequence [do
				valuePattern <- scan freeVars value
				return (name, SLTypeBinding valuePattern)
				| (name, SLTypeBinding value) <- M.toList typeBindings]
			return $ MetaObjectPattern (\obj -> case obj of
				SLTypeBinding type_' typeBindings' ->
					matchSLType type_ typeBindingPatterns type_' typeBindings'
				_ -> Nothing)
		scan freeVars (MOSLTerm term typeBindings termBindings) = do
			typeBindingPatterns <- liftM M.fromList $ sequence [do
				valuePattern <- scan freeVars value
				return (name, valuePattern)
				| (name, SLTypeBinding) <- M.toList typeBindings]
			termBindingPatterns <- liftM M.fromList $ sequence [do
				valuePattern <- scan (foldr M.delete freeVars (map fst params)) value
				return (name, (params, valuePattern))
				| (name, SLTermBinding params value) <- M.toList termBindings]
			return $ MetaObjectPattern (\obj -> case obj of
				SLTermBinding term' typeBindings' termBindings' ->
					matchSLTerm term typeBindingPatterns termBindingPatterms term' typeBindings' termBindings'
				_ -> Nothing)
		scan freeVars (MOJSExprTypeDefn defn params) = do
			paramPatterns <- mapM (scan freeVars params)
			return $ MetaObjectPattern (\obj -> case obj of
				MOJSExprTypeDefn defn' params' | nameOfJSExprTypeDefn defn == nameOfJSExprTypeDefn defn' -> do
					matches <- sequence [matchMetaObjectPattern pattern value
						| (pattern, value) <- zip paramPatterns params']
					foldM combineMatches M.empty matches
				_ -> Nothing)
		scan freeVars _ = fail ("can't match that type of pattern")
	pattern <- scan freeVars (reduceMetaObject object)
	let pattern' = MetaObjectPattern (\obj -> matchMetaObjectPattern pattern (reduceMetaObject obj))
	return pattern'

matchMetaObject :: 
                -> MetaObject
                -> Maybe (M.Map NameOfMetaObject MetaObject)
matchMetaObject freeVars pattern object = matchMetaObject' freeVars pattern [] object

matchMetaObject' :: M.Map NameOfMetaObject MetaType
                 -> MetaObject
                 -> [NameOfMetaObject]
                 -> MetaObject
                 -> Maybe (M.Map NameOfMetaObject MetaObject)
matchMetaObject' freeVars (MOName name _) patternArgs object | name `M.member` freeVars =
	return (M.singleton name 

matchMetaObject :: M.Map NameOfMetaObject MetaType
                -> MetaObject
                -> MetaObject
                -> Maybe (M.Map NameOfMetaObject MetaObject)
matchMetaObject freeVars (MOName name _) object | name `M.member` freeVars =
	return (M.singleton name object)
matchMetaObject freeVars (MOApp fun arg) object) = do
	let namesInFun = M.
