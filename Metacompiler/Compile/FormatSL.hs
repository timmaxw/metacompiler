module Metacompiler.Compile.FormatSL where

import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.Runtime as R
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.SL.ToSExpr as SL
import qualified Metacompiler.TL.Syntax as TL

formatSLKindAsSL :: R.SLKind -> SL.Kind ()
formatSLKindAsSL (R.SLKindType) =
	SL.KindType ()
formatSLKindAsSL (R.SLKindFun a r) =
	case formatSLKindAsSL r of
		SL.KindFun () as' r' -> SL.KindFun () (a':as') r'
		r' -> SL.KindFun () [a'] r'
	where a' = formatSLKindAsSL a

formatSLKindAsString :: R.SLKind -> String
formatSLKindAsString = SL.formatSLKindAsString . formatSLKindAsSL

formatMetaObjectAsSLType :: Monad m
                         => (M.Map R.NameOfSLType R.SLKind -> R.MetaObject -> m (SL.Type ()))
                         -> M.Map R.NameOfSLType R.SLKind
                         -> R.MetaObject
                         -> m (SL.Type ())
formatMetaObjectAsSLType tyf tys (R.MOSLTypeDefn defn) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType (R.nameOfSLDataDefn defn))))
formatMetaObjectAsSLType tyf tys (R.MOSLTypeName n k)
	| n `M.member` tys = return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType n)))
	| otherwise = tyf tys (R.MOSLTypeName n k)
formatMetaObjectAsSLType tyf tys (R.MOSLTypeApp f x) = do
	f' <- formatMetaObjectAsSLType tyf tys f
	x' <- formatMetaObjectAsSLType tyf tys x
	return (SL.TypeApp () f' x')
formatMetaObjectAsSLType tyf tys (R.MOSLTypeFun a r) = do
	a' <- formatMetaObjectAsSLType tyf tys a
	r' <- formatMetaObjectAsSLType tyf tys r
	case r' of
		SL.TypeFun () as' rest -> return (SL.TypeFun () (a':as') rest)
		_ -> return (SL.TypeFun () [a'] r')
formatMetaObjectAsSLType tyf tys (R.MOSLTypeLazy x) = do
	x' <- formatMetaObjectAsSLType tyf tys x
	return (SL.TypeLazy () x')
formatMetaObjectAsSLType tyf tys other =
	tyf tys other

formatMetaObjectAsSLTerm :: Monad m
                         => (M.Map R.NameOfSLType R.SLKind -> R.MetaObject -> m (SL.Type ()))
                         -> (M.Map R.NameOfSLType R.SLKind -> M.Map R.NameOfSLTerm R.MetaObject -> R.MetaObject -> m (SL.Term ()))
                         -> M.Map R.NameOfSLType R.SLKind
                         -> M.Map R.NameOfSLTerm R.MetaObject
                         -> R.MetaObject
                         -> m (SL.Term ())
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermDefn d typs) = do
	let d' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLTermDefn d))
	typs' <- mapM (formatMetaObjectAsSLType tyf tys) typs
	return (SL.TermName () d' typs')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermName n type_)
	| n `M.member` tes = return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm n)) [])
	| otherwise = tef tys tes (R.MOSLTermName n type_)
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermApp f x) = do
	f' <- formatMetaObjectAsSLTerm tyf tef tys tes f
	x' <- formatMetaObjectAsSLTerm tyf tef tys tes x
	return (SL.TermApp () f' x')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermAbs (a, at) b) = do
	let a' = SL.NameOfTerm (R.unNameOfSLTerm a)
	at' <- formatMetaObjectAsSLType tyf tys at
	b' <- formatMetaObjectAsSLTerm tyf tef tys (M.insert a at tes) b
	case b' of
		SL.TermAbs () as' rest -> return (SL.TermAbs () ((a', at'):as') rest)
		_ -> return (SL.TermAbs () [(a', at')] b')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermCase s cs) = do
	s' <- formatMetaObjectAsSLTerm tyf tef tys tes s
	cs' <- sequence [do
		let c' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		let fTypes = map ($ typs) (R.fieldTypesOfSLCtorDefn c)
		typs' <- mapM (formatMetaObjectAsSLType tyf tys) typs
		v' <- formatMetaObjectAsSLTerm tyf tef tys (M.fromList (zip fns fTypes) `M.union` tes) v
		return (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
	return (SL.TermCase () s' cs')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermData c typs teps) = do
	let c' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLCtorDefn c))
	typs' <- mapM (formatMetaObjectAsSLType tyf tys) typs
	teps' <- mapM (formatMetaObjectAsSLTerm tyf tef tys tes) teps
	return (foldl (SL.TermApp ()) (SL.TermName () c' typs') teps')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermWrap x) = do
	x' <- formatMetaObjectAsSLTerm tyf tef tys tes x
	return (SL.TermWrap () x')
formatMetaObjectAsSLTerm tyf tef tys tes (R.MOSLTermUnwrap x) = do
	x' <- formatMetaObjectAsSLTerm tyf tef tys tes x
	return (SL.TermUnwrap () x')
formatMetaObjectAsSLTerm tyf tef tys tes other =
	tef tys tes other

