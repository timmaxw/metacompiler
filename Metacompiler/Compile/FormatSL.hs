module Metacompiler.Compile.FormatSL where

import Control.Monad.Identity
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

formatSLTypeAsSL :: R.MetaObject -> SL.Type ()
formatSLTypeAsSL = runIdentity . formatSLTypeAsSL' handleTLType M.empty
	where handleTLType _ _ = return (SL.TypeName () (SL.NameOfType "<?>"))

formatSLTypeAsString :: R.MetaObject -> String
formatSLTypeAsString = SL.formatSLTypeAsString . formatSLTypeAsSL

formatSLTermAsSL :: R.MetaObject -> SL.Term ()
formatSLTermAsSL = runIdentity . formatSLTermAsSL' handleTLType handleTLTerm M.empty M.empty
	where
		handleTLType _ _ = return (SL.TypeName () (SL.NameOfType "<?>"))
		handleTLTerm _ _ _ = return (SL.TermName () (SL.NameOfTerm "<?>") [])

formatSLTermAsString :: R.MetaObject -> String
formatSLTermAsString = SL.formatSLTermAsString . formatSLTermAsSL

formatSLTypeAsSL' :: Monad m
                  => (M.Map R.NameOfSLType R.SLKind -> R.MetaObject -> m (SL.Type ()))
                  -> M.Map R.NameOfSLType R.SLKind
                  -> R.MetaObject
                  -> m (SL.Type ())
formatSLTypeAsSL' tyf tys (R.MOSLTypeDefn defn) =
	return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType (R.nameOfSLDataDefn defn))))
formatSLTypeAsSL' tyf tys (R.MOSLTypeName n k)
	| n `M.member` tys = return (SL.TypeName () (SL.NameOfType (R.unNameOfSLType n)))
	| otherwise = tyf tys (R.MOSLTypeName n k)
formatSLTypeAsSL' tyf tys (R.MOSLTypeApp f x) = do
	f' <- formatSLTypeAsSL' tyf tys f
	x' <- formatSLTypeAsSL' tyf tys x
	return (SL.TypeApp () f' x')
formatSLTypeAsSL' tyf tys (R.MOSLTypeFun a r) = do
	a' <- formatSLTypeAsSL' tyf tys a
	r' <- formatSLTypeAsSL' tyf tys r
	case r' of
		SL.TypeFun () as' rest -> return (SL.TypeFun () (a':as') rest)
		_ -> return (SL.TypeFun () [a'] r')
formatSLTypeAsSL' tyf tys (R.MOSLTypeLazy x) = do
	x' <- formatSLTypeAsSL' tyf tys x
	return (SL.TypeLazy () x')
formatSLTypeAsSL' tyf tys other =
	tyf tys other

formatSLTermAsSL' :: Monad m
                  => (M.Map R.NameOfSLType R.SLKind -> R.MetaObject -> m (SL.Type ()))
                  -> (M.Map R.NameOfSLType R.SLKind -> M.Map R.NameOfSLTerm R.MetaObject -> R.MetaObject -> m (SL.Term ()))
                  -> M.Map R.NameOfSLType R.SLKind
                  -> M.Map R.NameOfSLTerm R.MetaObject
                  -> R.MetaObject
                  -> m (SL.Term ())
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermDefn d typs) = do
	let d' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLTermDefn d))
	typs' <- mapM (formatSLTypeAsSL' tyf tys) typs
	return (SL.TermName () d' typs')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermName n type_)
	| n `M.member` tes = return (SL.TermName () (SL.NameOfTerm (R.unNameOfSLTerm n)) [])
	| otherwise = tef tys tes (R.MOSLTermName n type_)
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermApp f x) = do
	f' <- formatSLTermAsSL' tyf tef tys tes f
	x' <- formatSLTermAsSL' tyf tef tys tes x
	return (SL.TermApp () f' x')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermAbs (a, at) b) = do
	let a' = SL.NameOfTerm (R.unNameOfSLTerm a)
	at' <- formatSLTypeAsSL' tyf tys at
	b' <- formatSLTermAsSL' tyf tef tys (M.insert a at tes) b
	case b' of
		SL.TermAbs () as' rest -> return (SL.TermAbs () ((a', at'):as') rest)
		_ -> return (SL.TermAbs () [(a', at')] b')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermCase s cs) = do
	s' <- formatSLTermAsSL' tyf tef tys tes s
	cs' <- sequence [do
		let c' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLCtorDefn c))
		let fns' = [SL.NameOfTerm (R.unNameOfSLTerm n) | n <- fns]
		let fTypes = map ($ typs) (R.fieldTypesOfSLCtorDefn c)
		typs' <- mapM (formatSLTypeAsSL' tyf tys) typs
		v' <- formatSLTermAsSL' tyf tef tys (M.fromList (zip fns fTypes) `M.union` tes) v
		return (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
	return (SL.TermCase () s' cs')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermData c typs teps) = do
	let c' = SL.NameOfTerm (R.unNameOfSLTerm (R.nameOfSLCtorDefn c))
	typs' <- mapM (formatSLTypeAsSL' tyf tys) typs
	teps' <- mapM (formatSLTermAsSL' tyf tef tys tes) teps
	return (foldl (SL.TermApp ()) (SL.TermName () c' typs') teps')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermWrap x) = do
	x' <- formatSLTermAsSL' tyf tef tys tes x
	return (SL.TermWrap () x')
formatSLTermAsSL' tyf tef tys tes (R.MOSLTermUnwrap x) = do
	x' <- formatSLTermAsSL' tyf tef tys tes x
	return (SL.TermUnwrap () x')
formatSLTermAsSL' tyf tef tys tes other =
	tef tys tes other

