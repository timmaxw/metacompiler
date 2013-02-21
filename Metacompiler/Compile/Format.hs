module Metacompiler.Compile.Format where

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

formatMetaTypeAsTL :: R.MetaType -> TL.MetaType ()
formatMetaTypeAsTL (R.MTFun (pn, pt) rt) =
	case formatMetaTypeAsTL rt of
		TL.MTFun () ps' rt' -> TL.MTFun () ((pn', pt'):ps') rt'
		rt' -> TL.MTFun () [(pn', pt')] rt'
	where
		pn' = TL.Name (R.unName pn)
		pt' = formatMetaTypeAsTL pt
formatMetaTypeAsTL (R.MTSLType k) =
	TL.MTSLType () (formatSLKindAsSL k)
formatMetaTypeAsTL (R.MTSLTerm t) =
	TL.MTSLTerm () (formatMetaObjectAsTL t)
formatMetaTypeAsTL (R.MTJSExprType t) =
	TL.MTJSExprType () (formatMetaObjectAsTL t)
formatMetaTypeAsTL (R.MTJSExpr ty te) =
	TL.MTJSExpr () (formatMetaObjectAsTL ty) (formatMetaObjectAsTL te)

formatMetaObjectAsTL :: R.MetaObject -> TL.MetaObject ()
formatMetaObjectAsTL (R.MOApp f x) =
	TL.MOApp () (formatMetaObjectAsTL f) (formatMetaObjectAsTL x)
formatMetaObjectAsTL (R.MOAbs (pn, pt) b) =
	case formatMetaObjectAsTL b of
		TL.MOAbs () ps' b' -> TL.MOAbs () ((pn', pt'):ps') b'
		b' -> TL.MOAbs () [(pn', pt')] b'
	where
		pn' = TL.Name (R.unName pn)
		pt' = formatMetaTypeAsTL pt
formatMetaObjectAsTL (R.MOName (R.Name n) _) =
	TL.MOName () (TL.Name n)
formatMetaObjectAsTL mo@(R.MOSLTypeDefn _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeName _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeApp _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeFun _ _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTypeLazy _) =
	formatMetaObjectAsTLSLType mo
formatMetaObjectAsTL mo@(R.MOSLTermDefn _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermName _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermApp _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermAbs _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermCase _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermData _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermWrap _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL mo@(R.MOSLTermUnwrap _ _) =
	formatMetaObjectAsTLSLTerm mo
formatMetaObjectAsTL (R.MOJSExprTypeDefn d ps) =
	foldl (TL.MOApp ()) (TL.MOName 
formatMetaObjectAsTL (R.MOJSExprLiteral e t c bs) =
	...

formatMetaObjectAsTLSLType :: R.MetaObject -> TL.MetaObject ()

formatMetaObjectAsTLSLTerm :: R.MetaObject -> TL.MetaObject () 
