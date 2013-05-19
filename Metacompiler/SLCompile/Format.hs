module Metacompiler.SLCompile.Format where

import Control.Monad.Identity
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Metacompiler.SLRuntime.Types as SLR
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.SLSyntax.ToSExpr as SLS

formatKindAsSyntax :: SLR.Kind -> SLS.Kind ()
formatKindAsSyntax (SLR.KindType) =
	SLS.KindType ()
formatKindAsSyntax (SLR.KindFun a r) =
	case formatKindAsSyntax r of
		SLS.KindFun () as' r' -> SLS.KindFun () (a':as') r'
		r' -> SLS.KindFun () [a'] r'
	where a' = formatKindAsSyntax a

formatKindAsString :: SLR.Kind -> String
formatKindAsString = SLS.formatKindAsString . formatKindAsSyntax

formatTypeAsSyntax :: SLR.Type -> SLS.Type ()
formatTypeAsSyntax (SLR.TypeDefn defn) =
	SLS.TypeName () (SLS.NameOfType (SLR.unNameOfType (SLR.nameOfDataDefn defn)))
formatTypeAsSyntax (SLR.TypeName name _) =
	SLS.TypeName () (SLS.NameOfType (SLR.unNameOfType name))
formatTypeAsSyntax (SLR.TypeApp f x) =
	SLS.TypeApp () (formatTypeAsSyntax f) (formatTypeAsSyntax x)
formatTypeAsSyntax (SLR.TypeFun a r) =
	case formatTypeAsSyntax r of
		SLS.TypeFun () as' r' -> SLS.TypeFun () (a':as') r'
		_ -> SLS.TypeFun () [a'] r'
	where a' = formatTypeAsSyntax a
formatTypeAsSyntax (SLR.TypeLazy x) =
	SLS.TypeLazy () (formatTypeAsSyntax x)

formatTypeAsString :: SLR.Type -> String
formatTypeAsString = SLS.formatTypeAsString . formatTypeAsSyntax

formatTermAsSyntax' :: SLR.Term -> SLS.Term ()
formatTermAsSyntax (SLR.TermDefn d typs) =
	SLS.TermName () d' (map formatTypeAsSyntax typs)
	where d' = SLS.NameOfTerm (SLR.unNameOfTerm (SLR.nameOfTermDefn d))
formatTermAsSyntax (SLR.TermName n _) =
	SLS.TermName () (SLS.NameOfTerm (SLR.unNameOfTerm n))
formatTermAsSyntax (SLR.TermApp f x) =
	SLS.TermApp () (formatTermAsSyntax f) (formatTermAsSyntax x)
formatTermAsSyntax (SLR.TermAbs (a, at) b) =
	case formatTermAsSyntax b of
		SLS.TermAbs () as' b' -> SLS.TermAbs () ((a', at'):as') b'
		_ -> SLS.TermAbs () [(a', at')] b'
	where
		a' = SLS.NameOfTerm (SLR.unNameOfTerm a)
		at' = formatTypeAsSyntax at
formatTermAsSyntax (SLR.TermCase s cs) =
	SLS.TermCase ()
		(formatTermAsSyntax s)
		[let
			c' = SLS.NameOfTerm (SLR.unNameOfTerm (SLR.nameOfCtorDefn c))
			typs' = map formatTypeAsSyntax typs
			fns' = [SLS.NameOfTerm (SLR.unNameOfTerm n) | n <- fns]
			v' = formatTermAsSyntax v
		in (c', typs', fns', v')
		| (c, typs, fns, v) <- cs]
formatTermAsSyntax (SLR.TermData c typs teps) =
	foldl
		(SLS.TermApp ())
		(SLS.TermName ()
			(SLS.NameOfTerm (SLR.unNameOfTerm (SLR.nameOfCtorDefn c)))
			(map formatTypeAsSyntax typs))
		(map formatTermAsSyntax teps)
formatTermAsSyntax (SLR.TermWrap x) =
	SLS.TermWrap () (formatTermAsSyntax x)
formatTermAsSyntax (SLR.TermUnwrap x) =
	SLS.TermUnwap () (formatTermAsSyntax x)

formatTermAsString :: SLR.Term -> String
formatTermAsString = SLS.formatTermAsString . formatTermAsSyntax

