module Metacompiler.TLCompile.Format where

import Control.Monad
import Control.Monad.State
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import qualified Metacompiler.SLCompile.Format as SLF
import qualified Metacompiler.TLRuntime.TLRuntime as TLR
import qualified Metacompiler.TLSyntax.ToSExpr as TLS
import qualified Metacompiler.TLSyntax.Types as TLS

formatMetaTypeAsSyntax :: TLR.MetaType -> TLS.MetaType ()
formatMetaTypeAsSyntax (TLR.MTFun (pn, pt) rt) =
	case formatMetaTypeAsSyntax rt of
		TLS.MTFun () ps' rt' -> TLS.MTFun () ((pn', pt'):ps') rt'
		rt' -> TLS.MTFun () [(pn', pt')] rt'
	where
		pn' = TLS.Name (TLR.unNameOfMetaObject pn)
		pt' = formatMetaTypeAsSyntax pt
formatMetaTypeAsSyntax (TLR.MTSLType k) =
	TLS.MTSLType () (SLF.formatSLKindAsSyntax k)
formatMetaTypeAsSyntax (TLR.MTSLTerm t) =
	TLS.MTSLType () (formatMetaObjectAsSyntax t)
formatMetaTypeAsSyntax (TLR.MTJSExprType t) =
	TLS.MTJSExprType () (formatMetaObjectAsSyntax t')
formatMetaTypeAsSyntax (TLR.MTJSExpr ty te) =
	TLS.MTJSExpr () (formatMetaObjectAsSyntax ty) (formatMetaObjectAsSyntax te)

formatMetaTypeAsString :: TLR.MetaType -> String
formatMetaTypeAsString = TLS.formatMetaTypeAsString . formatMetaTypeAsSyntax

formatMetaObjectAsSyntax :: TLR.MetaObject -> TLFMonad (TLS.MetaObject ())
formatMetaObjectAsSyntax (TLR.MOApp f x) =
	TLS.MOApp () (formatMetaObjectAsSyntax f) (formatMetaObjectAsSyntax x)
formatMetaObjectAsSyntax (TLR.MOAbs (pn, pt) b) = do
	case formatMetaTypeAsSyntax b of
		TLS.MOAbs () ps' b' -> TLS.MOAbs () ((pn', pt'):ps') b'
		b' -> TLS.MOAbs () [(pn', pt')] b'
	where
		pn' = TLS.Name (TLR.unNameOfMetaObject pn)
		pt' = formatMetaTypeAsSyntax pt
formatMetaObjectAsSyntax (TLR.MOName n _) =
	TLS.MOName () (TLS.Name (TLR.unNameOfMetaObject n))
formatMetaObjectAsSyntax (TLR.MOSLType type_ bs) =
	TLS.MOSLType ()
		(SLF.formatTypeAsSyntax type_)
		[TLS.Binding ()
			(SLS.NameOfType (SLR.unNameOfType n))
			[]
			(formatMetaObjectAsSyntax v)
			| (n, TLR.SLTypeBinding v) <- M.toList bs]
formatMetaObjectAsSyntax (TLR.MOSLTerm term tybs tebs) =
	TLS.MOSLTerm ()
		(SLF.formatTermAsSyntax term)
		[TLS.Binding ()
			(SLS.NameOfType (SLR.unNameOfType n))
			[]
			(formatMetaObjectAsSyntax v)
			| (n, TLR.SLTypeBinding v) <- M.toList tybs]
		[TLS.Binding ()
			(SLS.NameOfTerm (SLR.unNameOfTerm n))
			[(TLS.Name (TLR.unNameOfMetaObject pn), TLS.MTSLTerm () (formatMetaObjectAsSyntax pt)) | (pn, pt) <- ps]
			(formatMetaObjectAsSyntax v)
			| (n, TLR.SLTermBinding ps v) <- M.toList tebs]
formatMetaObjectAsSyntax (TLR.MOJSExprTypeDefn d ps) =
	foldl (TLS.MOApp ()) (TLS.MOName () d') (map formatMetaObjectAsSyntax ps)
	where d' = TLS.Name (TLR.unNameOfMetaObject (TLR.nameOfJSExprTypeDefn d))
formatMetaObjectAsSyntax (TLR.MOJSExprLiteral e t c bs) =
	TLS.MOJSExprLiteral ()
		(formatMetaObjectAsSyntax e)
		(formatMetaObjectAsSyntax t)
		(JS.reannotate (const undefined) c)
		[TLS.Binding ()
			n
			[let
				n1' = TLS.Name (TLR.unNameOfMetaObject n1)
				n2' = TLS.Name (TLR.unNameOfMetaObject n2)
				ps' = [(n1', TLS.MTSLTerm () (formatMetaObjectAsSyntax t1)),
					(n2', TLS.MTJSExpr () (formatMetaObjectAsSyntax t2) (TLS.MOName () n1'))]
				in TLS.BindingParam () ps'
				| TLR.JSExprBindingParam n1 t1 n2 t2 <- ps]
			(formatMetaObjectAsSyntax v)
			| (n, TLR.JSExprBinding ps v) <- M.toList bs]
formatMetaObjectAsSyntax (TLR.MOJSExprConvertEquiv outEquiv content) =
	TLS.MOJSExprConvertEquiv ()
		(formatMetaObjectAsSyntax outEquiv)
		(formatMetaObjectAsSyntax (case TLR.typeOfMetaObject content of TLR.MTJSExpr _ equiv -> equiv))
		(formatMetaObjectAsSyntax content)

formatMetaObjectAsString :: TLR.MetaObject -> String
formatMetaObjectAsString = TLS.formatMetaObjectAsString . formatMetaObjectAsSyntax

