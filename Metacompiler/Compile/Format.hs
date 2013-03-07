module Metacompiler.Compile.Format where

import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL



formatMetaTypeAsTL :: R.MetaType -> TL.MetaType ()
formatMetaTypeAsTL obj =
	case runState (formatMetaTypeAsTL' (metaObjectsInNames (freeAndGlobalNamesInMetaType obj)) obj) (TLFS [] []) of
		(obj', TLFS [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL obj =
	case runState (formatMetaObjectAsTL' (metaObjectsInNames (freeAndGlobalNamesInMetaObject obj)) obj) (TLFS [] []) of
		(obj', TLFS [] []) -> obj'
		_ -> error "unbound SL type/term variables at top level"



-- `formatMetaObjectAsSLType` and `formatMetaObjectAsSLTerm` run in the `SLFS` monad. Like with `TLFS`, the fields
-- `forbiddenTypeNamesInSLFS` and `forbiddenTermNamesInSLFS` are both read and added to, but the fields
-- `typeBindingsInSLFS` and `termBindingsInSLFS` are append-only.
--
-- Suppose that running `(formatMetaObjectAsSLTerm someTerm)` produces the result `someTerm'` while adding `types` and
-- `terms` to the `typeBindingsInSLFS` and `termBindingsInSLFS` fields of the state. Then if `someTerm'` is evaluated
-- with each key in `types` bound to its value and each key in `terms` bound to its value, the result will be
-- equivalent to the original `someTerm`.






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
	SLFS tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFS [] []) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFS [] [])
	let binding = TL.Binding () name [] other'
	put (SLFS (tybs ++ [binding]) tebs)
	return (SL.TypeName () name)

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
	b' <- formatMetaObjectAsSLTerm (S.insert a' ftens) b
	return (SL.TermAbs [(a', at')] b')
formatMetaObjectAsSLTerm (R.MOSLTermCase s cs) = do
	s' <- formatMetaObjectAsSLTerm s
	cs' <- sequence [do
		let c' = SL.NameOfCtor (R.unNameOfSLCtor (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		typs' <- mapM formatMetaObjectAsSLType typs
		v' <- formatMetaObjectAsSLTerm (ftens `S.union` S.fromList fns') v
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
	SLFS tybs tebs <- get
	let candidates = [SL.NameOfType ([c] ++ n)
		| c <- "abcdefghijklmnopqrstuvwxyz", n <- [""] ++ map show [2..]]
	let ftyns' = ftyns `S.union` S.fromList [name | TL.Binding () name _ _ <- tybs]
	let Just name = find (`S.notMember` ftyns') candidates
	(other', TLFS typs teps) <-
		runState (formatMetaObjectAsTL (namesInFreeNames (freeAndGlobalNamesInMetaObject other)) other) (TLFS [] [])
	let binding = TL.Binding () name (map fst typs ++ map fst teps) other'
	put (SLFS tybs (tebs ++ [binding]))
	return (foldl (S.TermApp ()) (S.TermName name (map snd typs)) (map snd teps))

