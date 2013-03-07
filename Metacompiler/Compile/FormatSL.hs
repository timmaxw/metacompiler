module Metacompiler.Compile.FormatSL where

import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL

formatSLKindAsSL :: R.SLKind -> SL.Kind ()
formatSLKindAsSL (R.SLKindType) =
	SL.KindType ()
formatSLKindAsSL (R.SLKindFun a r) =
	case formatSLKindAsSL r of
		SL.KindFun () as' r' -> SL.KindFun (a':as') r'
		r' -> SL.KindFun () [a'] r'
	where a' = formatSLKindAsSL a

formatMetaObjectAsSLType :: R.MetaObject -> (SL.Type (), [TL.Binding () SL.NameOfType], EmbeddedSL)

formatMetaObjectAsSLTerm :: R.MetaObject -> (SL.Term (), [TL.Binding () SL.NameOfType], [TL.Binding () SL.NameOfTerm], EmbeddedSL)

data EmbeddedTL = EmbeddedTL {
	typesInEmbeddedTL :: [TL.Binding () SL.NameOfType],
	termsInEmbeddedTL :: [TL.Binding () SL.NameOfTerm]
	}

---------------------------------------

data SLFMonad a = SLFMonad {
    runSLFMonad :: ([SL.NameOfTerm], S.Set SL.NameOfType, S.Set SL.NameOfTerm)
                -> TLFMonad (S.Set SL.NameOfType, S.Set SL.NameOfTerm, EmbeddedTL, a)
    }

instance Monad SLFMonad where
	return x = SLFMonad $
		\ (stack, forbiddenTypes, forbiddenTerms) ->
			return (forbiddenTypes, forbiddenTerms, EmbeddedTL [] [], x)
	a >>= b = SLFMonad $ \ (stack, fTypes1, fTerms1) ->
		runSLFMonad a (stack, fTypes1, fTerms1) >>=
			\ (fTypes2, fTerms2, EmbeddedTL types1 terms1, x) ->
				runSLFMonad (b x) (stack, fTypes2, fTerms2) >>=
					\ (fTypes3, fTerms3, EmbeddedTL types2 terms2, y) ->
						return (fTypes3, fTerms3, EmbeddedTL (types1++types2) (terms1++terms2), y)

addNamesToScopeForSLFMonad :: [SL.NameOfTerm] -> SLFMonad a -> SLFMonad a
addNamesToScopeForSLFMonad names inner =
	SLFMonad (\ (stack, fTypes, fTerms) -> runSLFMonad inner (stack ++ names, fTypes, fTerms))

generateTypeNameForSLFMonad :: SLFMonad SL.NameOfType
generateTypeNameForSLFMonad = SLFMonad $ \ (stack, fTypes, fTerms) -> let
	candidates = [SL.NameOfType ([c] ++ n) | c <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ", n <- [""] ++ map show [2..]]
	Just name = Data.List.find (`S.notMember` fTypes) candidates
	in return (S.insert name fTypes, fTerms, EmbeddedTL M.empty M.empty, name)

generateTermNameForSLFMonad :: SLFMonad SL.NameOfType
generateTermNameForSLFMonad = SLFMonad $ \ (stack, fTypes, fTerms) -> let
	candidates = [SL.NameOfType ([c] ++ n) | c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	fTerms' = fTerms `S.union` S.fromList stack
	Just name = Data.List.find (`S.notMember` fTerms') candidates
	in return (fTypes, S.insert name fTerms, EmbeddedTL M.empty M.empty, name)

recordEmbeddedTLForSLFMonad :: EmbeddedTL -> SLFMonad ()
recordEmbeddedTLForSLFMonad etl =
	SLFMonad $ \ (stack, fTypes, fTerms) -> return (fTypes, fTerms, etl, ())

recordEmbeddedSLForSLFMonad :: EmbeddedSL -> SLFMonad ()
recordEmbeddedSLForSLFMonad etl =
	SLFMonad $ \ (stack, fTypes, fTerms) -> do
		recordEmbeddedSLForTLFMonad etl
		return (fTypes, fTerms, EmbeddedTL M.empty M.empty, ())

embedTLFMonadInSLFMonad :: TLFMonad a -> SLFMonad (a, EmbeddedSL)
embedTLFMonadInSLFMonad inner =
	SLFMonad $ \ (slStack, fTypes, fTerms) ->
		TLFMonad $ \ (tlStack, tlForbidden) ->
			case runTLFMonad inner (tlStack, tlForbidden) of
				(tlForbidden', esl, x) ->
					(tlForbidden', EmbeddedSL M.empty M.empty, (fTypes, fTerms, EmbeddedTL [] [], (x, esl)))

---------------------------------------

formatMetaObjectAsSLType :: R.MetaObject -> State SLFS (SL.Type ())
formatMetaObjectAsSLType (R.MOSLTypeDefn defn) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType (R.nameOfSLDataDefn defn))))
formatMetaObjectAsSLType (R.MOSLTypeName n k) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType n)))
formatMetaObjectAsSLType (R.MOSLTypeApp f x) = do
	f' <- formatMetaObjectAsSLType f
	x' <- formatMetaObjectAsSLType x
	return (SL.TypeApp f' x')
formatMetaObjectAsSLType (R.MOSLTypeFun a r) = do
	a' <- formatMetaObjectAsSLType a
	r' <- formatMetaObjectAsSLType r
	return (SL.TypeFun a' r')
formatMetaObjectAsSLType (R.MOSLTypeLazy x) = do
	x' <- formatMetaObjectAsSLType ftyns x
	return (SL.TypeLazy x')
formatMetaObjectAsSLType other = do
	(other', EmbeddedSL eslTypes eslTerms) <- embedTLFMonadInSLFMonad (formatMetaObjectAsTL other)
	typeParams <- liftM concat $ sequence [do
		let FreeNamesAndTypes fnMetaObjects fnSLTypes fnSLTerms = freeNamesInMetaObject value
		unless (M.null fnMetaObjects) (error "please don't embed SL terms with free MO names")
		
		| (name, value) <- M.fromList eslTypes]

formatMetaObjectAsSLTerm :: R.MetaObject -> State SLFS (SL.Term ())
formatMetaObjectAsSLTerm (R.MOSLTermDefn d typs) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLTermDefn d))))
formatMetaObjectAsSLTerm (R.MOSLTermName n _) =
	return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm n)))
formatMetaObjectAsSLTerm (R.MOSLTermApp f x) = do
	f' <- formatMetaObjectAsSLTerm f
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermApp () f' x')
formatMetaObjectAsSLTerm (R.MOSLTermAbs (a, at) b) = do
	let a' = SL.NameOfTerm (R.unNameOfSLType a)
	at' <- formatMetaObjectAsSLType at
	b' <- addTermNamesToScopeForSLFS [a'] $ formatMetaObjectAsSLTerm b
	return (SL.TermAbs [(a', at')] b')
formatMetaObjectAsSLTerm (R.MOSLTermCase s cs) = do
	s' <- formatMetaObjectAsSLTerm s
	cs' <- sequence [do
		let c' = SL.NameOfCtor (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		typs' <- mapM formatMetaObjectAsSLType typs
		v' <- addTermNamesToScopeForSLFS fns' $ formatMetaObjectAsSLTerm v
		return (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
	return (SL.TermCase s' cs')
formatMetaObjectAsSLTerm (R.MOSLTermData c typs teps) = do
	let c' = SL.NameOfTerm (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
	typs' <- mapM formatMetaObjectAsSLType typs
	teps' <- mapM formatMetaObjectAsSLType teps
	return (foldl (SL.TermApp ()) (SL.TermName () c' typs') teps)
formatMetaObjectAsSLTerm (R.MOSLTermWrap x) = do
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermWrap () x')
formatMetaObjectAsSLTerm (R.MOSLTermUnwrap x) = do
	x' <- formatMetaObjectAsSLTerm x
	return (SL.TermUnwrap () x')
formatMetaObjectAsSLTerm other = do
	...

