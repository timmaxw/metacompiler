module Metacompiler.TLSyntax where

import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Metacompiler.SLSyntax as SL

-- `MetaType` represents a translation-language meta-type. It is more of a
-- syntactic representation than a semantic one; for example, it doesn't
-- represent `fun ... -> ...` blocks with multiple arguments down into multiple
-- single-argument blocks. It is parameterized on a "tag" type, which will
-- typically be `Range`.

data MetaType a
	-- The meta-type of unparameterized Javascript types:
	--     'js-type'
	= MTJSType {
		tagOfMetaType :: a
	}

	-- The meta-type of unparameterized terms:
	--     'js-term' (<type>)
	| MTJSTerm {
		tagOfMetaType:: a,
		typeOfMetaType :: MetaObject a
	}

	-- The meta-type of parameterized meta-objects:
	--     'fun' (<meta-type>)+ '->' <meta-type>
	| MTFun {
		tagOfMetaType :: a,
		paramsOfMetaType :: [(String, MetaType a)],
		resultOfMetaType :: MetaType a
	}

	deriving Show

-- `MetaObject` represents a translation-language meta-object.

data MetaObject a
	-- Meta-object application:
	--     <meta-object> (<meta-object>)
	= MOApp {
		tagOfMetaObject :: a,
		funOfMetaObject :: MetaObject a,
		argOfMetaObject :: MetaObject a
	}

	-- Meta-object abstraction:
	--     '\' (<name> '::' (<meta-type>))+ '->' <meta-object>
	| MOAbs {
		tagOfMetaObject :: a,
		paramsOfMetaObject :: [(String, MetaType a)],
		resultOfMetaObject :: MetaObject a
	}

	-- Referring to a meta-object in scope:
	--     <name>
	| MOVar {
		tagOfMetaObject :: a,
		varOfMetaObject :: String
	}

	-- Term with a manually-specified JS equivalent:
	--     ('js-expr' "<JS-code>" '::' <type>
	--         ('spec' <SL-term>)
	--         ('=' "<JS-var>" <value>)
	--     )
	| MOJSExpr {
		tagOfMetaObject :: a,
		codeOfMetaObject :: JS.Expression JS.SourcePos,
		typeOfMetaObject :: MetaObject a,
		specOfMetaObject :: Maybe (SL.Term a),
		subsOfMetaObject :: [(String, MetaObject a)]
	}

	deriving Show

-- `Directive` represents a top-level translation language directive.

data Directive a
	-- `let` directive:
	--     ('let' <name> (<name> '::' <meta-type>)* {'::' (<meta-type>)}? '='
	--         <meta-object>
	--     )
	= DLet {
		tagOfDirective :: a,
		nameOfDirective :: String,
		paramsOfDirective :: [(String, MetaType a)],
		typeOfDirective :: Maybe (MetaType a),
		valueOfDirective :: MetaObject a
	}

	-- `js-repr` directive:
	--     ('js-repr' <name> (<name> '::' <meta-type>)* '='
	--         ('spec' <SL-type>)
	--     )
	| DJSRepr {
		tagOfDirective :: a,
		nameOfDirective :: String,
		paramsOfDirective :: [(String, MetaType a)],
		specOfDirective :: SL.Type a
	}

	deriving Show


