module TestMetacompiler.SLRuntime where

import Metacompiler.SExpr.Parse
import Metacompiler.SExpr.Types
import qualified Metacompiler.SLCompile.Compile as SLC
import qualified Metacompiler.SLSyntax.FromSExpr as SLS
import qualified Metacompiler.SLSyntax.Types as SLS
import qualified Metacompiler.SLCompile.Compile as SLC
import qualified Metacompiler.TLCompile.Compile as TLC
import qualified Metacompiler.TLCompile.Format as TLF
import qualified Metacompiler.TLRuntime.TLRuntime as TLR
import qualified Metacompiler.TLSyntax.FromSExpr as TLS
import qualified Metacompiler.TLSyntax.Types as TLS
import qualified Metacompiler.TLSyntax.ToSExpr as TLS
import TestMetacompiler.Error

compileSLDirectives :: String -> SLC.Defns
compileSLDirectives str = assumeSuccess ("in compileSLDirectives " ++ show str) $ do
	sexprs <- parseSExprs str
	dirs <- mapM SLS.parseSLDirFromSExpr (sExprsToList sexprs)
	SLC.compileSLDirectives dirs

defaultSLDefns :: SLC.Defns
defaultSLDefns = compileSLDirectives "(data Nat = (Zero) (Succ Nat))"



