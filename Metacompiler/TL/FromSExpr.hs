module Metacompiler.TL.FromSExpr where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Parser as JS
import qualified Metacompiler.JS as JS
import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Parse
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsFrom
import qualified Metacompiler.SL.FromSExpr as SL
import qualified Metacompiler.SL.Syntax as SL
import qualified Metacompiler.TL.Syntax as TL
import qualified Text.Parsec
import Text.Parsec.String ()   -- So `String` is an instance of `Stream`

-- `parseTLDirectiveFromSExpr` tries to interpret an S-expression as a
-- `TL.Directive`. If it doesn't work, then it returns `Left <error>`.

parseTLDirectiveFromSExpr :: SExpr -> ErrorMonad (TL.Directive Range)

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "let") exprs1to7)) = do
    {- sample input:
    (let name (arg :: type) :: type = value )
        ^    ^             ^  ^    ^ ^     ^
        1    2             3  4    5 6     7
    -}
	(name, exprs2to7) <- case exprs1to7 of
		Cons (Atom _ n) r -> return (TL.Name n, r)
		_ -> fail ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs exprs1to7)))
	(exprs2to5, exprs6to7) <- breakOnAtom "=" exprs2to7
	(exprs2to3, maybeType) <- case breakOnAtom "::" exprs2to5 of
		Failure _ -> return (exprs2to5, Nothing)
		Success (ps, t) -> case t of
			Cons ty (Nil _) -> do
				ty' <- parseTLMetaTypeFromSExpr ty
				return (ps, Just ty')
			_ -> fail ("expected single-atom type after \"::\" at " ++
				formatPoint (startOfRange (rangeOfSExprs t)) ++ " but instead got " ++
				formatSExprsForMessage t)
	params <- mapM parseTLParameterFromSExpr (sExprsToList exprs2to3)
	value <- parseTLMetaObjectFromSExprs exprs6to7
	return $ TL.DLet {
		TL.tagOfDirective = range,
		TL.nameOfDLet = name,
		TL.paramsOfDLet = params,
		TL.typeOfDLet = maybeType,
		TL.valueOfDLet = value
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "sl-code") exprs1to2)) = do
    {- sample input:
    (sl-code "foo" )
            ^     ^
            1     2
    -}
	expr1to2 <- case exprs1to2 of
		Cons something (Nil _) -> return something
		_ -> fail ("expected `(sl-code \"<string>\")`")
	content <- parseSLDirectivesFromString expr1to2
	return $ TL.DSLCode {
		TL.tagOfDirective = range,
		TL.contentOfDSLCode = content
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-expr-type") exprs1to7)) = do
    {- sample input:
    (js-expr-type name (arg :: type) = (spec blah ) )
                 ^    ^             ^ ^     ^    ^ ^
                 1    2             3 4     5    6 7
    -}
	(name, exprs2to7) <- case exprs1to7 of
		Cons (Atom _ n) r -> return (TL.Name n, r)
		_ -> fail ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs exprs1to7)))
	(exprs2to3, exprs4to7) <- breakOnAtom "=" exprs2to7
	params <- mapM parseTLParameterFromSExpr (sExprsToList exprs2to3)
	clauses <- parseClausesFromSExprs [("spec", False, False)] exprs4to7
	slEquiv <- let [(range4to7, exprs5to6)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs exprs5to6
	return $ TL.DJSExprType {
		TL.tagOfDirective = range,
		TL.nameOfDJSExprType = name,
		TL.paramsOfDJSExprType = params,
		TL.slEquivOfDJSExprType = slEquiv
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-expr-global") exprs1to14)) = do
    {- sample input:
    (js-expr-global name (arg :: type) = (args blah ) (type blah ) (spec blah ) (body blah )  )
                   ^    ^             ^ ^     ^    ^ ^     ^    ^       ^    ^       ^    ^  ^
                   1    2             3 4     5    6 7     8    9       10   11      12   13 14
    -}
	(name, exprs2to14) <- case exprs1to14 of
		Cons (Atom _ n) r -> return (TL.Name n, r)
		_ -> fail ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs exprs1to14)))
	(exprs2to3, exprs4to14) <- breakOnAtom "=" exprs2to14
	params <- mapM parseTLParameterFromSExpr (sExprsToList exprs2to3)
	clauses <- parseClausesFromSExprs
		[("args", True, True), ("type", False, False), ("spec", False, False), ("body", False, False)]
		exprs4to14
	runtimeArgs <- liftM concat $ sequence [
		sequence [case expr5to6 of
			Atom _ n -> return (TL.Name n)
			_ -> fail ("everything after the `args` in an `(args ...)` clause should be the name of a parameter, but \
				\at " ++ formatRange (rangeOfSExpr expr5to6) ++ " there is something that is not an atom.")
			| expr5to6 <- sExprsToList exprs5to6]
		| (range4to7, exprs5to6) <- (M.!) clauses "args"]
	type_ <- let [(_, exprs8to9)] = (M.!) clauses "type" in
		parseTLMetaObjectFromSExprs exprs8to9
	spec <- let [(_, exprs10to11)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs exprs10to11
	body <- let [(_, exprs12to13)] = (M.!) clauses "body" in
		parseTLMetaObjectFromSExprs exprs12to13
	return $ TL.DJSExprGlobal {
		TL.tagOfDirective = range,
		TL.nameOfDJSExprGlobal = name,
		TL.paramsOfDJSExprGlobal = params,
		TL.runtimeArgsOfDJSExprGlobal = runtimeArgs,
		TL.typeOfDJSExprGlobal = type_,
		TL.specOfDJSExprGlobal = spec,
		TL.bodyOfDJSExprGlobal = body
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-emit") exprs1to5)) = do
	{- sample input:
	(js-emit "code" (expr name = value ) )
	        ^      ^     ^            ^ ^
	        1      2     3            4 5
	-}
	(expr1to2, exprs2to5) <- takeOne "code" exprs1to5
	code <- parseJavaScriptStatementsFromString expr1to2
	clauses <- parseClausesFromSExprs [("expr", True, True)] exprs2to5
	bindings <- forM ((M.!) clauses "expr") $ \ (range2to5, exprs3to4) ->
		parseBindingFromSExprs (JS.Id ()) range2to5 exprs3to4
	return $ TL.DJSEmit {
		TL.tagOfDirective = range,
		TL.codeOfDJSEmit = code,
		TL.bindingsOfDJSEmit = bindings
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom r name) _)) =
	fail ("directive at " ++ formatRange range ++ " has invalid directive type \"" ++ name ++ "\"")

parseTLDirectiveFromSExpr other =
	fail ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

-- `parseTLParameterFromSExpr` expects a `SExpr` that looks like
-- `(name :: type)`. It parses the name and type and returns them as a tuple.
-- It is used in `fun ... -> ...` types, `\ ... -> ...` terms, and at the top
-- of `let` and `js-expr` directives.

parseTLParameterFromSExpr :: SExpr -> ErrorMonad (TL.Name, TL.MetaType Range)
parseTLParameterFromSExpr (List r (Cons (Atom _ name) (Cons (Atom _ "::") ty))) = do
	ty' <- parseTLMetaTypeFromSExprs ty
	return (TL.Name name, ty')
parseTLParameterFromSExpr other =
	fail ("invalid parameter: expected `(name :: type)`, got " ++ formatSExprForMessage other ++
		" at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMultiParameterFromSExpr` expects a `SExpr` that looks like
-- `(name1 :: type1 | name2 :: type2 | name3 :: type3 | ...)`. It's used in
-- bindings.

parseTLMultiParameterFromSExpr :: SExpr -> ErrorMonad [(TL.Name, TL.MetaType Range)]
parseTLMultiParameterFromSExpr (List r l) = do
	parts <- multiBreakOnAtom "|" l
	forM parts $ \ part -> case part of
		Cons (Atom _ name) (Cons (Atom _ "::") ty) -> do
			ty' <- parseTLMetaTypeFromSExprs ty
			return (TL.Name name, ty')
		_ -> fail ("invalid parameter: expected `name :: type`, got " ++ formatSExprsForMessage part ++
			" at " ++ formatRange (rangeOfSExprs part))

-- `parseClausesFromSExprs` expects a series of clauses of the form
-- `(keyword ...)`. The first parameter specifies the allowed keywords, and how
-- many times each one must appear; the first boolean specifies if it's OK for
-- the keyword to be missing, and the second specifies if it's OK for the
-- keyword to appear multiple times. The results are returned unparsed.

parseClausesFromSExprs :: [(String, Bool, Bool)] -> SExprs -> ErrorMonad (M.Map String [(Range, SExprs)])
parseClausesFromSExprs spec seq = do
	clauses <- parseClauses' seq
	sequence [do
		let numFound = length ((M.!) clauses name)
		when (numFound == 0 && not zeroOk) $
			fail ("missing mandatory clause \"" ++ name ++ "\" at " ++ formatRange (rangeOfSExprs seq))
		when (numFound > 1 && not multiOk) $
			fail ("clause \"" ++ name ++ "\" should not appear more than once in " ++ formatRange (rangeOfSExprs seq))
		| (name, zeroOk, multiOk) <- spec]
	return clauses
	where
		parseClauses' :: SExprs -> ErrorMonad (M.Map String [(Range, SExprs)])
		parseClauses' (Nil _) = return (M.fromList [(name, []) | (name, _, _) <- spec])
		parseClauses' (Cons c rest) = do
			(name, value) <- case c of
				List _ (Cons (Atom _ name) value) -> return (name, value)
				_ -> fail ("invalid clause " ++ formatSExprForMessage c ++ " at " ++ formatRange (rangeOfSExpr c))
			unless (any (\(name', _, _) -> name == name') spec) $
				fail ("invalid clause type \"" ++ name ++ "\" at " ++ formatRange (rangeOfSExpr c))
			rest' <- parseClauses' rest
			return (M.adjust ((rangeOfSExpr c, value):) name rest')

-- `parseTLMetaTypeFromSExpr` tries to interpret an S-expression as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExpr :: SExpr -> ErrorMonad (TL.MetaType Range)
parseTLMetaTypeFromSExpr (List _ stuff) =
	parseTLMetaTypeFromSExprs stuff
parseTLMetaTypeFromSExpr other =
	fail ("invalid meta-type " ++ formatSExprForMessage other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaTypeFromSExprs` tries to interpret a list of S-expressions as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExprs :: SExprs -> ErrorMonad (TL.MetaType Range)
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-type") exprs1to2) = do
    {- sample input:
    (sl-type "kind" )
            ^      ^
            1      2
    -}
	expr1to2 <- expectOne "kind" exprs1to2
	kind <- parseSLKindFromString expr1to2
	return $ TL.MTSLType {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slKindOfMTSLType = kind
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-term") exprs1to2) = do
	{- sample input:
    (sl-term type )
            ^    ^
            1    2
    -}
	expr1to2 <- expectOne "type" exprs1to2
	type_ <- parseTLMetaObjectFromSExpr expr1to2
	return $ TL.MTSLTerm {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slTypeOfMTSLTerm = type_
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "fun") exprs1to4) = do
	{- sample input:
	(fun (param :: type) -> type )
	    ^               ^  ^    ^
	    1               2  3    4
	-}     
	(exprs1to2, exprs3to4) <- breakOnAtom "->" exprs1to4
	params <- mapM parseTLParameterFromSExpr (sExprsToList exprs1to2)
	body <- parseTLMetaTypeFromSExprs exprs3to4
	return $ TL.MTFun {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.paramsOfMTFun = params,
		TL.resultOfMTFun = body
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-expr-type") exprs1to2) = do
	{- sample input:
	(js-expr-type type )
	             ^    ^
	             1    2
	-}
	expr1to2 <- expectOne "type" exprs1to2
	equiv <- parseTLMetaObjectFromSExpr expr1to2
	return $ TL.MTJSExprType {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slTypeOfMTJSExprType = equiv
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-expr") exprs1to3) = do
    {- sample input:
    (js-expr type equiv )
            ^    ^     ^
            1    2     3
    -}
	(expr1to2, exprs2to3) <- takeOne "type" exprs1to3
	expr2to3 <- expectOne "equivalent" exprs2to3
	type_ <- parseTLMetaObjectFromSExpr expr1to2
	equiv <- parseTLMetaObjectFromSExpr expr2to3
	return $ TL.MTJSExpr {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.jsTypeOfMTJSExpr = type_,
		TL.slTermOfMTJSExpr = equiv
		}
parseTLMetaTypeFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaTypeFromSExpr x
parseTLMetaTypeFromSExprs other =
	fail ("invalid meta-type " ++ formatSExprsForMessage other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseTLMetaObjectFromSExpr` tries to interpret a list of S-expressions as a
-- `TL.MetaObject`.

parseTLMetaObjectFromSExpr :: SExpr -> ErrorMonad (TL.MetaObject Range)
parseTLMetaObjectFromSExpr (Atom range x) =
	return $ TL.MOName {
		TL.tagOfMetaObject = range,
		TL.varOfMOName = TL.Name x
		}
parseTLMetaObjectFromSExpr (List _ stuff) =
	parseTLMetaObjectFromSExprs stuff
parseTLMetaObjectFromSExpr other =
	fail ("invalid meta-object " ++ formatSExprForMessage other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaObjectFromSExprs` tries to interpret a list of S-expressions as
-- a `TL.MetaObject`.

parseTLMetaObjectFromSExprs :: SExprs -> ErrorMonad (TL.MetaObject Range)
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "\\") exprs1to4) = do
	{- sample input:
	(\ (arg :: type) -> body )
	  ^             ^  ^    ^
	  1             2  3    4
	-}
	(exprs1to2, exprs3to4) <- breakOnAtom "->" exprs1to4
	args <- mapM parseTLParameterFromSExpr (sExprsToList exprs1to2)
	body <- parseTLMetaObjectFromSExprs exprs3to4
	return $ TL.MOAbs {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.paramsOfMOAbs = args,
		TL.resultOfMOAbs = body
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-type") exprs1to5) = do
    {- sample input:
    (sl-type "type" (type "name" (arg :: type) = value ) )
            ^      ^     ^                            ^ ^
            1      2     3                            4 5
    -}
	(expr1to2, exprs2to5) <- case exprs1to5 of
		Nil p -> fail ("expected a SL type at " ++ formatPoint p)
		Cons expr1to2 exprs2to5 -> return (expr1to2, exprs2to5)
	code <- parseSLTypeFromString expr1to2
	clauses <- parseClausesFromSExprs [("type", True, True)] exprs2to5
	bindings <- forM ((M.!) clauses "type") $ \ (range2to5, exprs3to4) ->
		parseBindingFromSExprs SL.NameOfType range2to5 exprs3to4
	return $ TL.MOSLTypeLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.codeOfMOSLTypeLiteral = code,
		TL.typeBindingsOfMOSLTypeLiteral = bindings
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-term") exprs1to8) = do
	{- sample input:
    (sl-type "term" (type "name" (arg :: type) = value ) (term "name" (arg :: type) = value ) )
            ^      ^     ^                            ^ ^     ^                            ^ ^
            1      2     3                            4 5     6                            7 8
    -}
	(expr1to2, exprs2to8) <- case exprs1to8 of
		Nil p -> fail ("expected a SL term at " ++ formatPoint p)
		Cons expr1to2 exprs2to8 -> return (expr1to2, exprs2to8)
	code <- parseSLTermFromString expr1to2
	clauses <- parseClausesFromSExprs [("type", True, True), ("term", True, True)] exprs2to8
	typeBindings <- forM ((M.!) clauses "type") $ \ (range2to5, exprs3to4) ->
		parseBindingFromSExprs SL.NameOfType range2to5 exprs3to4
	termBindings <- forM ((M.!) clauses "term") $ \ (range5to8, exprs6to7) ->
		parseBindingFromSExprs SL.NameOfTerm range5to8 exprs6to7
	return $ TL.MOSLTermLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.codeOfMOSLTermLiteral = code,
		TL.typeBindingsOfMOSLTermLiteral = typeBindings,
		TL.termBindingsOfMOSLTermLiteral = termBindings
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr") exprs1to11) = do
	{- sample input:
	(js-expr (type blah ) (spec blah ) (impl "code" (expr "name" (x :: type | y :: type) = value ) )  )
	        ^     ^    ^       ^    ^       ^      ^     ^                                      ^ ^  ^
	        1     2    3       4    5       6      7     8                                      9 10 11
	-}
	clauses <- parseClausesFromSExprs [("type", False, False), ("spec", False, False), ("impl", False, False)] exprs1to11
	type_ <- let [(_, exprs2to3)] = (M.!) clauses "type" in
		parseTLMetaObjectFromSExprs exprs2to3
	spec <- let [(_, exprs4to5)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs exprs4to5
	(code, bindings) <- let [(_, exprs6to10)] = (M.!) clauses "impl" in do
		(expr6to7, exprs7to10) <- takeOne "code" exprs6to10
		code <- parseJavaScriptExpressionFromString expr6to7
		clauses' <- parseClausesFromSExprs [("expr", True, True)] exprs7to10
		bindings <- forM ((M.!) clauses' "expr") $ \ (range7to10, exprs8to9) ->
			parseBindingFromSExprs (JS.Id ()) range7to10 exprs8to9
		return (code, bindings)
	return $ TL.MOJSExprLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.slTermOfMOJSExprLiteral = spec,
		TL.jsTypeOfMOJSExprLiteral = type_,
		TL.codeOfMOJSExprLiteral = code,
		TL.bindingsOfMOJSExprLiteral = bindings
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr-convert-spec") exprs1to8) = do
	{- sample input:
	(js-expr-convert-spec (in-spec blah ) (out-spec blah ) (content blah ) )
	                     ^        ^    ^           ^    ^          ^    ^ ^
	                     1        2    3           4    5          6    7 8
	-}
	clauses <- parseClausesFromSExprs [("in-spec", False, False), ("out-spec", False, False), ("content", False, False)] exprs1to8
	inSpec <- let [(_, exprs2to3)] = (M.!) clauses "in-spec" in
		parseTLMetaObjectFromSExprs exprs2to3
	outSpec <- let [(_, exprs4to5)] = (M.!) clauses "out-spec" in
		parseTLMetaObjectFromSExprs exprs4to5
	content <- let [(_, exprs6to7)] = (M.!) clauses "content" in
		parseTLMetaObjectFromSExprs exprs6to7
	return (TL.MOJSExprConvertEquiv {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.contentOfMOJSExprConvertEquiv = content,
		TL.inEquivOfMOJSExprConvertEquiv = inSpec,
		TL.outEquivOfMOJSExprConvertEquiv = outSpec
		})
parseTLMetaObjectFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaObjectFromSExpr x
parseTLMetaObjectFromSExprs whole@(Cons first args) = do
	first' <- parseTLMetaObjectFromSExpr first
	args' <- sequence [do
		arg' <- parseTLMetaObjectFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExpr first)
	return (foldl (\ f (a, endPoint) ->
		TL.MOApp {
			TL.tagOfMetaObject = Range startPoint endPoint,
			TL.funOfMOApp = f,
			TL.argOfMOApp = a
			}
		) first' args')
parseTLMetaObjectFromSExprs other =
	fail ("invalid meta-object " ++ formatSExprsForMessage other ++ " at " ++ formatRange (rangeOfSExprs other))

parseBindingFromSExprs :: (String -> n) -> Range -> SExprs -> ErrorMonad (TL.Binding Range n)
parseBindingFromSExprs nameMaker range exprs1to5 = do
	{- sample input:
	 "name" (arg :: type) = value
	^      ^             ^ ^     ^
	1      2             3 4     5
	-}
	(expr1to2, exprs2to5) <- takeOne "name" exprs1to5
	name <- case expr1to2 of
		Quoted _ s -> return (nameMaker s)
		_ -> fail ("at " ++ formatRange (rangeOfSExpr expr1to2) ++ ": name of variable to bind should be quoted")
	(exprs2to3, exprs4to5) <- breakOnAtom "=" exprs2to5
	params <- sequence [do
		parts <- parseTLMultiParameterFromSExpr exprs2to3'
		return (TL.BindingParam (rangeOfSExpr exprs2to3') parts)
		| exprs2to3' <- sExprsToList exprs2to3]
	value <- parseTLMetaObjectFromSExprs exprs4to5
	return (TL.Binding {
		TL.tagOfBinding = range,
		TL.nameOfBinding = name,
		TL.paramsOfBinding = params,
		TL.valueOfBinding = value
		})

-- `parseJavaScriptExpressionFromString` and `parseJavaScriptStatementsFromString` try to interpret the given string as
-- a JavaScript expression.

parseJavaScriptExpressionFromString :: SExpr -> ErrorMonad (JS.Expression JS.SourcePos)
parseJavaScriptExpressionFromString (Quoted _ string) =
	case JS.parse JS.parseExpression "<string>" string' of
		Left err -> fail (show err)
		Right x -> return x
	where
		string' = dropWhile isSpace string
parseJavaScriptExpressionFromString other =
	fail ("expected a quoted string with JavaScript code")

parseJavaScriptStatementsFromString :: SExpr -> ErrorMonad [JS.Statement JS.SourcePos]
parseJavaScriptStatementsFromString (Quoted _ string) =
	case JS.parse (JS.parseStatement `Text.Parsec.sepBy` Text.Parsec.space) "<string>" string of
		Left err -> fail (show err)
		Right x -> return x
	where
		string' = dropWhile isSpace string
parseJavaScriptStatementsFromString other =
	fail ("expected a quoted string with JavaScript code")

-- `parseSL{Kind,Type,Term,Directives}FromString` try to interpret the given string as a SL object. SL is also
-- expressed as S-expressions, but it should still appear in quotes for consistency with Javascript.

parseSLKindFromString :: SExpr -> ErrorMonad (SL.Kind Range)
parseSLKindFromString (Quoted _ string) = do
	sexprs <- parseSExprs string
	SL.parseSLKindFromSExprs sexprs

parseSLTypeFromString :: SExpr -> ErrorMonad (SL.Type Range)
parseSLTypeFromString (Quoted _ string) = do
	sexprs <- parseSExprs string
	SL.parseSLTypeFromSExprs sexprs

parseSLTermFromString :: SExpr -> ErrorMonad (SL.Term Range)
parseSLTermFromString (Quoted _ string) = do
	sexprs <- parseSExprs string
	SL.parseSLTermFromSExprs sexprs

parseSLDirectivesFromString :: SExpr -> ErrorMonad [SL.Dir Range]
parseSLDirectivesFromString (Quoted _ string) = do
	sexprs <- parseSExprs string
	mapM SL.parseSLDirFromSExpr (sExprsToList sexprs)

