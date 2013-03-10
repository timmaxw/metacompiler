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

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "let") rest)) = do
	(name, rest2) <- case rest of
		Cons (Atom _ n) r -> return (n, r)
		_ -> fail ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs rest)))
	(unparsedParamsAndMaybeType, rest3) <- breakOnAtom "=" rest2
	(unparsedParams, maybeType) <- case breakOnAtom "::" unparsedParamsAndMaybeType of
		Failure _ -> return (unparsedParamsAndMaybeType, Nothing)
		Success (ps, t) -> case t of
			Cons ty (Nil _) -> do
				ty' <- parseTLMetaTypeFromSExpr ty
				return (ps, Just ty')
			_ -> fail ("expected single-atom type after \"::\" at " ++
				formatPoint (startOfRange (rangeOfSExprs t)) ++ " but instead got " ++
				summarizeSExprs t)
	params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
	value <- parseTLMetaObjectFromSExprs rest3
	return $ TL.DLet {
		TL.tagOfDirective = range,
		TL.nameOfDLet = TL.Name name,
		TL.paramsOfDLet = params,
		TL.typeOfDLet = maybeType,
		TL.valueOfDLet = value
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "sl-code") rest)) = do
	rest2 <- case rest of
		Cons something (Nil _) -> return something
		_ -> fail ("expected `(sl-code \"<string>\")`")
	content <- parseSLDirectivesFromString rest2
	return $ TL.DSLCode {
		TL.tagOfDirective = range,
		TL.contentOfDSLCode = content
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-expr-type") rest)) = do
	(name, rest2) <- case rest of
		Cons (Atom _ n) r -> return (n, r)
		_ -> fail ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs rest)))
	(unparsedParams, rest3) <- breakOnAtom "=" rest2
	params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
	clauses <- parseClausesFromSExprs [("spec", False, False)] rest3
	slEquiv <- let [(range, slEquiv)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs slEquiv
	return $ TL.DJSExprType {
		TL.tagOfDirective = range,
		TL.nameOfDJSExprType = TL.Name name,
		TL.paramsOfDJSExprType = params,
		TL.slEquivOfDJSExprType = slEquiv
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-emit") rest)) = do
	(code1, bindings1) <- takeOne "code" rest
	code2 <- parseJavaScriptStatementsFromString code1
	bindings2 <- parseClausesFromSExprs [("expr", True, True)] bindings1
	bindings3 <- forM ((M.!) bindings2 "expr") $ \ (range, body) ->
		parseBindingFromSExprs (JS.Id ()) range body
	return $ TL.DJSEmit {
		TL.tagOfDirective = range,
		TL.codeOfDJSEmit = code2,
		TL.bindingsOfDJSEmit = bindings3
		}

parseTLDirectiveFromSExpr (List range (Cons (Atom r name) rest)) =
	fail ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)

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
	fail ("invalid parameter: expected `(name :: type)`, got " ++ summarizeSExpr other ++
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
		_ -> fail ("invalid parameter: expected `name :: type`, got " ++ summarizeSExprs part ++
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
				_ -> fail ("invalid clause " ++ summarizeSExpr c ++ " at " ++ formatRange (rangeOfSExpr c))
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
	fail ("invalid meta-type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaTypeFromSExprs` tries to interpret a list of S-expressions as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExprs :: SExprs -> ErrorMonad (TL.MetaType Range)
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-type") rest) = do
	unparsedKind <- expectOne "kind" rest
	kind <- parseSLKindFromString unparsedKind
	return $ TL.MTSLType {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slKindOfMTSLType = kind
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-term") rest) = do
	unparsedType <- expectOne "type" rest
	type_ <- parseTLMetaObjectFromSExpr unparsedType
	return $ TL.MTSLTerm {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slTypeOfMTSLTerm = type_
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(unparsedParams, unparsedBody) <- breakOnAtom "->" rest
	params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
	body <- parseTLMetaTypeFromSExprs unparsedBody
	return $ TL.MTFun {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.paramsOfMTFun = params,
		TL.resultOfMTFun = body
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-expr-type") rest) = do
	unparsedEquiv <- expectOne "type" rest
	equiv <- parseTLMetaObjectFromSExpr unparsedEquiv
	return $ TL.MTJSExprType {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.slTypeOfMTJSExprType = equiv
		}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-expr") rest) = do
	(unparsedType, rest2) <- takeOne "type" rest
	unparsedEquiv <- expectOne "equivalent" rest2
	type_ <- parseTLMetaObjectFromSExpr unparsedType
	equiv <- parseTLMetaObjectFromSExpr unparsedEquiv
	return $ TL.MTJSExpr {
		TL.tagOfMetaType = rangeOfSExprs whole,
		TL.jsTypeOfMTJSExpr = type_,
		TL.slTermOfMTJSExpr = equiv
		}
parseTLMetaTypeFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaTypeFromSExpr x
parseTLMetaTypeFromSExprs other =
	fail ("invalid meta-type " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

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

-- `parseTLMetaObjectFromSExprs` tries to interpret a list of S-expressions as
-- a `TL.MetaObject`.

parseTLMetaObjectFromSExprs :: SExprs -> ErrorMonad (TL.MetaObject Range)
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "\\") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- mapM parseTLParameterFromSExpr (sExprsToList args)
	body <- parseTLMetaObjectFromSExprs rest2
	return $ TL.MOAbs {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.paramsOfMOAbs = args',
		TL.resultOfMOAbs = body
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-type") rest) = do
	(code, bindings1) <- case rest of
		Nil p -> fail ("expected a SL type at " ++ formatPoint p)
		Cons code bindings1 -> return (code, bindings1)
	code' <- parseSLTypeFromString code
	bindings2 <- parseClausesFromSExprs [("type", True, True)] bindings1
	bindings3 <- forM ((M.!) bindings2 "type") $ \ (range, body) ->
		parseBindingFromSExprs SL.NameOfType range body
	return $ TL.MOSLTypeLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.codeOfMOSLTypeLiteral = code',
		TL.typeBindingsOfMOSLTypeLiteral = bindings3
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-term") rest) = do
	(code, bindings1) <- case rest of
		Nil p -> fail ("expected a SL term at " ++ formatPoint p)
		Cons code bindings1 -> return (code, bindings1)
	code' <- parseSLTermFromString code
	bindings2 <- parseClausesFromSExprs [("type", True, True), ("term", True, True)] bindings1
	typeBindings3 <- forM ((M.!) bindings2 "type") $ \ (range, body) ->
		parseBindingFromSExprs SL.NameOfType range body
	termBindings3 <- forM ((M.!) bindings2 "term") $ \ (range, body) ->
		parseBindingFromSExprs SL.NameOfTerm range body
	return $ TL.MOSLTermLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.codeOfMOSLTermLiteral = code',
		TL.typeBindingsOfMOSLTermLiteral = typeBindings3,
		TL.termBindingsOfMOSLTermLiteral = termBindings3
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr") rest) = do
	clauses <- parseClausesFromSExprs [("type", False, False), ("spec", False, False), ("impl", False, False)] rest
	type_ <- let [(_, unparsedType)] = (M.!) clauses "type" in
		parseTLMetaObjectFromSExprs unparsedType
	spec <- let [(_, unparsedSpec)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs unparsedSpec
	(code, bindings) <- let [(_, unparsedCodeAndBindings)] = (M.!) clauses "impl" in do
		(code1, bindings1) <- takeOne "code" unparsedCodeAndBindings
		code2 <- parseJavaScriptExpressionFromString code1 
		bindings2 <- parseClausesFromSExprs [("expr", True, True)] bindings1
		bindings3 <- forM ((M.!) bindings2 "expr") $ \ (range, body) ->
			parseBindingFromSExprs (JS.Id ()) range body
		return (code2, bindings3)
	return $ TL.MOJSExprLiteral {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.slTermOfMOJSExprLiteral = spec,
		TL.jsTypeOfMOJSExprLiteral = type_,
		TL.codeOfMOJSExprLiteral = code,
		TL.bindingsOfMOJSExprLiteral = bindings
		}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr-loop-break") rest) = do
	clauses <- parseClausesFromSExprs [("type", False, False), ("spec", False, False), ("content", False, False)] rest
	type_ <- let [(_, unparsedType)] = (M.!) clauses "type" in
		parseTLMetaObjectFromSExprs unparsedType
	spec <- let [(_, unparsedSpec)] = (M.!) clauses "spec" in
		parseTLMetaObjectFromSExprs unparsedSpec
	content <- let [(_, unparsedContent)] = (M.!) clauses "content" in
		parseTLMetaObjectFromSExprs unparsedContent
	return (TL.MOJSExprLoopBreak {
		TL.tagOfMetaObject = rangeOfSExprs whole,
		TL.contentOfMOJSExprLoopBreak = content,
		TL.jsTypeOfMOJSExprLoopBreak = type_,
		TL.slTermOfMOJSExprLoopBreak = spec
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
	fail ("invalid meta-object " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

parseBindingFromSExprs :: (String -> n) -> Range -> SExprs -> ErrorMonad (TL.Binding Range n)
parseBindingFromSExprs nameMaker range rest1 = do
	(unparsedName, rest2) <- takeOne "name" rest1
	name <- case unparsedName of
		Quoted _ s -> return (nameMaker s)
		_ -> fail ("at " ++ formatRange (rangeOfSExpr unparsedName) ++ ": name of variable to bind should be quoted")
	(unparsedParams, unparsedValue) <- breakOnAtom "=" rest2
	params <- sequence [do
		parts <- parseTLMultiParameterFromSExpr unparsedParam
		return (TL.BindingParam (rangeOfSExpr unparsedParam) parts)
		| unparsedParam <- sExprsToList unparsedParams]
	value <- parseTLMetaObjectFromSExprs unparsedValue
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

