module Metacompiler.SExprToTL where

import Control.Monad
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Parser as JS
import qualified Language.ECMAScript3.Syntax as JS
import Metacompiler.SExpr
import Metacompiler.SExprToSL as SL
import qualified Metacompiler.SLSyntax as SL
import qualified Metacompiler.TLSyntax as TL
import qualified Text.Parsec
import Text.Parsec.String ()   -- So `String` is an instance of `Stream`

-- `parseTLDirectiveFromSExpr` tries to interpret an S-expression as a
-- `TL.Directive`. If it doesn't work, then it returns `Left <error>`.

parseTLDirectiveFromSExpr :: SExpr -> Either String (TL.Directive Range)

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "let") rest)) =
	errorContext ("in \"let\" directive at " ++ formatRange range) $ do
		(name, rest2) <- case rest of
			Cons (Atom _ n) r -> return (n, r)
			_ -> Left ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs rest)))
		(unparsedParamsAndMaybeType, rest3) <- breakOnAtom "=" rest2
		(unparsedParams, maybeType) <- case breakOnAtom "::" unparsedParamsAndMaybeType of
			Left e -> return (unparsedParamsAndMaybeType, Nothing)
			Right (ps, t) -> case t of
				Cons ty (Nil _) -> do
					ty' <- parseTLMetaTypeFromSExpr ty
					return (ps, Just ty')
				_ -> Left ("expected single-atom type after \"::\" at " ++
					formatPoint (startOfRange (rangeOfSExprs t)) ++ " but instead got " ++
					summarizeSExprs t)
		params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
		value <- parseTLMetaObjectFromSExprs rest3
		return $ TL.DLet {
			TL.tagOfDirective = range,
			TL.nameOfDLet = name,
			TL.paramsOfDLet = params,
			TL.typeOfDLet = maybeType,
			TL.valueOfDLet = value
			}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "sl-code") rest)) =
	errorContext ("in \"sl-code\" directive at " ++ formatRange range) $ do
		content <- mapM SL.parseSLDirFromSExpr (sExprsToList rest)
		return $ TL.DSLCode {
			TL.tagOfDirective = range,
			TL.contentOfDSLCode = content
			}

{-
parseTLDirectiveFromSExpr (List range (Cons (Atom _ "js-repr") rest)) =
	errorContext ("in \"js-repr\" directive at " ++ formatRange range) $ do
		(name, rest2) <- case rest of
			Cons (Atom _ n) r -> return (n, r)
			_ -> Left ("missing or invalid name at " ++ formatPoint (startOfRange (rangeOfSExprs rest)))
		(unparsedParams, rest3) <- breakOnAtom "=" rest2
		params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
		clauses <- parseClausesFromSExprs [("spec", False, False)] rest3
		spec <- let [(range, spec)] = (M.!) clauses "spec" in
			errorContext ("in \"spec\" clause at " ++ formatRange range) $
			parseSLTypeFromSExprs spec
		return $ TL.DJSRepr {
			TL.tagOfDirective = range,
			TL.nameOfDirective = name,
			TL.paramsOfDirective = params,
			TL.specOfDirective = spec
			}

parseTLDirectiveFromSExpr (List range (Cons (Atom _ "emit") rest)) =
	errorContext ("in \"emit\" directive at " ++ formatRange range) $ do
		(codeRange, unparsedCode, rest') <- case rest of
			Cons (Quoted codeRange unparsedCode) rest' ->
				return (codeRange, unparsedCode, rest')
			_ -> Left ("expected (emit \"<code>\" <clauses...>)")
		code <-
			errorContext ("in code at " ++ formatRange codeRange) $
			parseJavaScriptStatementsFromString unparsedCode
		clauses <- parseClausesFromSExprs [("=", True, True)] rest'
		subs <- sequence [do
			(name, value) <- case rest of
				Cons (Quoted _ name) value -> return (name, value)
				_ -> Left ("malformed (= ...) clause at " ++ formatRange range ++
					"; expected (= \"<name>\" <value>).")
			value' <- parseTLMetaObjectFromSExprs value
			return (name, value')
			| (range, rest) <- (M.!) clauses "="]
		return $ TL.DEmit {
			TL.tagOfDirective = range,
			TL.codeOfDirective = code,
			TL.subsOfDirective = subs
			}
-}

parseTLDirectiveFromSExpr (List range (Cons (Atom r name) rest)) =
	Left ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)

parseTLDirectiveFromSExpr other =
	Left ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

-- `parseTLParameterFromSExpr` expects a `SExpr` that looks like
-- `(name :: type)`. It parses the name and type and returns them as a tuple.
-- It is used in `fun ... -> ...` types, `\ ... -> ...` terms, and at the top
-- of `let` and `js-expr` directives.

parseTLParameterFromSExpr :: SExpr -> Either String (TL.Name, TL.MetaType Range)
parseTLParameterFromSExpr (List r (Cons (Atom _ name) (Cons (Atom _ "::") ty))) = do
	ty' <-
		errorContext ("in type of parameter `" ++ name ++ "` at " ++ formatRange r) $
		parseTLMetaTypeFromSExprs ty
	return (TL.Name name, ty')
parseTLParameterFromSExpr other =
	Left ("invalid parameter: expected `(name :: type)`, got " ++ summarizeSExpr other ++
		" at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMultiParameterFromSExpr` expects a `SExpr` that looks like
-- `(name1 :: type1 | name2 :: type2 | name3 :: type3 | ...)`. It's used in
-- bindings.

parseTLMultiParameterFromSExpr :: SExpr -> Either String [(TL.Name, TL.MetaType Range)]
parseTLMultiParameterFromSExpr (List r l) = do
	parts <- multiBreakOnAtom "|" l
	forM parts $ \ part -> case part of
		Cons (Atom _ name) (Cons (Atom _ "::") ty) -> do
			ty' <- errorContext ("in type of parameter `" ++ name ++ "` at " ++ formatRange (rangeOfSExprs part)) $
				parseTLMetaTypeFromSExprs ty
			return (TL.Name name, ty')
		_ -> Left ("invalid parameter: expected `name :: type`, got " ++ summarizeSExprs part ++
			" at " ++ formatRange (rangeOfSExprs part))

-- `parseClausesFromSExprs` expects a series of clauses of the form
-- `(keyword ...)`. The first parameter specifies the allowed keywords, and how
-- many times each one must appear; the first boolean specifies if it's OK for
-- the keyword to be missing, and the second specifies if it's OK for the
-- keyword to appear multiple times. The results are returned unparsed.

parseClausesFromSExprs :: [(String, Bool, Bool)] -> SExprs -> Either String (M.Map String [(Range, SExprs)])
parseClausesFromSExprs spec seq = do
	clauses <- parseClauses' seq
	sequence [do
		let numFound = length ((M.!) clauses name)
		when (numFound == 0 && not zeroOk) $
			Left ("missing mandatory clause \"" ++ name ++ "\" at " ++ formatRange (rangeOfSExprs seq))
		when (numFound > 1 && not multiOk) $
			Left ("clause \"" ++ name ++ "\" should not appear more than once in " ++ formatRange (rangeOfSExprs seq))
		| (name, zeroOk, multiOk) <- spec]
	return clauses
	where
		parseClauses' :: SExprs -> Either String (M.Map String [(Range, SExprs)])
		parseClauses' (Nil _) = return (M.fromList [(name, []) | (name, _, _) <- spec])
		parseClauses' (Cons c rest) = do
			(name, value) <- case c of
				List _ (Cons (Atom _ name) value) -> return (name, value)
				_ -> Left ("invalid clause " ++ summarizeSExpr c ++ " at " ++ formatRange (rangeOfSExpr c))
			unless (any (\(name', _, _) -> name == name') spec) $
				Left ("invalid clause type \"" ++ name ++ "\" at " ++ formatRange (rangeOfSExpr c))
			rest' <- parseClauses' rest
			return (M.adjust ((rangeOfSExpr c, value):) name rest')

-- `parseTLMetaTypeFromSExpr` tries to interpret an S-expression as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExpr :: SExpr -> Either String (TL.MetaType Range)
{-
parseTLMetaTypeFromSExpr (Atom range "js-type") =
	return $ TL.MTJSType {
		TL.tagOfMetaType = range
		}
-}
parseTLMetaTypeFromSExpr (List _ stuff) =
	parseTLMetaTypeFromSExprs stuff
parseTLMetaTypeFromSExpr other =
	Left ("invalid meta-type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaTypeFromSExprs` tries to interpret a list of S-expressions as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExprs :: SExprs -> Either String (TL.MetaType Range)
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-type") rest) =
	errorContext ("in `(sl-type ...)` meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		unparsedKind <- expectOne "kind" rest
		kind <- SL.parseSLKindFromSExpr unparsedKind
		return $ TL.MTSLType {
			TL.tagOfMetaType = rangeOfSExprs whole,
			TL.slKindOfMTSLType = kind
			}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "sl-term") rest) =
	errorContext ("in `(sl-term ...)` meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		unparsedType <- expectOne "type" rest
		type_ <- parseTLMetaObjectFromSExpr unparsedType
		return $ TL.MTSLTerm {
			TL.tagOfMetaType = rangeOfSExprs whole,
			TL.slTypeOfMTSLTerm = type_
			}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "fun") rest) =
	errorContext ("in \"fun\" meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		(unparsedParams, unparsedBody) <- breakOnAtom "->" rest
		params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
		body <- parseTLMetaTypeFromSExprs unparsedBody
		return $ TL.MTFun {
			TL.tagOfMetaType = rangeOfSExprs whole,
			TL.paramsOfMTFun = params,
			TL.resultOfMTFun = body
			}
{-
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-equiv-expr-type") rest) =
	...
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "js-equiv-expr") rest) =
	...
-}
parseTLMetaTypeFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaTypeFromSExpr x
parseTLMetaTypeFromSExprs other =
	Left ("invalid meta-type " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseTLMetaObjectFromSExpr` tries to interpret a list of S-expressions as a
-- `TL.MetaObject`.

parseTLMetaObjectFromSExpr :: SExpr -> Either String (TL.MetaObject Range)
parseTLMetaObjectFromSExpr (Atom range x) =
	return $ TL.MOName {
		TL.tagOfMetaObject = range,
		TL.varOfMOName = TL.Name x
		}
parseTLMetaObjectFromSExpr (List _ stuff) =
	parseTLMetaObjectFromSExprs stuff

-- `parseTLMetaObjectFromSExprs` tries to interpret a list of S-expressions as
-- a `TL.MetaObject`.

parseTLMetaObjectFromSExprs :: SExprs -> Either String (TL.MetaObject Range)
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "\\") rest) =
	errorContext ("in abstraction " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $ do
		(args, rest2) <- breakOnAtom "->" rest
		args' <- mapM parseTLParameterFromSExpr (sExprsToList args)
		body <- parseTLMetaObjectFromSExprs rest2
		return $ TL.MOAbs {
			TL.tagOfMetaObject = rangeOfSExprs whole,
			TL.paramsOfMOAbs = args',
			TL.resultOfMOAbs = body
			}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-type") rest) =
	errorContext ("in `(sl-type ...)` at " ++ formatRange (rangeOfSExprs whole)) $ do
		(code, bindings1) <- case rest of
			Nil p -> Left ("expected a SL type at " ++ formatPoint p)
			Cons code bindings1 -> return (code, bindings1)
		code' <- SL.parseSLTypeFromSExpr code
		bindings2 <- parseClausesFromSExprs [("type", True, True)] bindings1
		bindings3 <- forM ((M.!) bindings2 "type") $ \ (range, body) ->
			parseBindingFromSExprs SL.NameOfType range body
		return $ TL.MOSLTypeLiteral {
			TL.tagOfMetaObject = rangeOfSExprs whole,
			TL.codeOfMOSLTypeLiteral = code',
			TL.typeBindingsOfMOSLTypeLiteral = bindings3
			}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "sl-term") rest) =
	errorContext ("in `(sl-term ...)` at " ++ formatRange (rangeOfSExprs whole)) $ do
		(code, bindings1) <- case rest of
			Nil p -> Left ("expected a SL term at " ++ formatPoint p)
			Cons code bindings1 -> return (code, bindings1)
		code' <- SL.parseSLTermFromSExpr code
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

{-
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr") rest) =
	...
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-global") rest) =
	errorContext ("in \"js-global\" at " ++ formatRange (rangeOfSExprs whole)) $ do
		(unparsedClauses, unparsedContent) <- case sExprsInitAndLast rest of
			Just (unparsedClauses, unparsedContent) ->
				return (unparsedClauses, unparsedContent)
			_ -> Left ("expected (js-global <clauses...> \"<code>\")")
		clauses <- parseClausesFromSExprs [("type", False, False), ("spec", True, False)] unparsedClauses
		type_ <- let [(_, unparsedType)] = (M.!) clauses "type" in
			errorContext ("in type at " ++ formatRange (rangeOfSExprs unparsedType)) $
			parseTLMetaObjectFromSExprs unparsedType
		spec <- case (M.!) clauses "spec" of
			[(range, rest)] ->
				errorContext ("in (spec ...) clause at " ++ formatRange range) $ do
					spec <- parseSLTermFromSExprs rest
					return (Just spec)
			[] -> return Nothing
		content <- parseTLMetaObjectFromSExpr unparsedContent
		return (TL.MOJSGlobal {
			TL.tagOfMetaObject = rangeOfSExprs whole,
			TL.uniqueIdOfMetaObject = TL.JSGlobalUniqueId (formatRange (rangeOfSExprs whole)),
			TL.contentOfMetaObject = content,
			TL.typeOfMetaObject = type_,
			TL.specOfMetaObject = spec
			})
-}
parseTLMetaObjectFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaObjectFromSExpr x
parseTLMetaObjectFromSExprs whole@(Cons first args) = do
	first' <-
		errorContext ("in function being called in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		parseTLMetaObjectFromSExpr first
	args' <- sequence [do
		arg' <-
			errorContext ("in argument #" ++ show i ++ " in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
			parseTLMetaObjectFromSExpr arg
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
	Left ("invalid meta-object " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

parseBindingFromSExprs :: (String -> n) -> Range -> SExprs -> Either String (TL.Binding Range n)
parseBindingFromSExprs nameMaker range rest1 = do
	(unparsedName, rest2) <- takeOne "name" rest1
	name <- case unparsedName of
		Quoted _ s -> return (nameMaker s)
		_ -> Left ("at " ++ formatRange (rangeOfSExpr unparsedName) ++ ": name of variable to bind should be quoted")
	(unparsedParams, unparsedValue) <- breakOnAtom "=" rest2
	params <- mapM (liftM TL.BindingParam . parseTLMultiParameterFromSExpr) (sExprsToList unparsedParams)
	value <- parseTLMetaObjectFromSExprs unparsedValue
	return (TL.Binding {
		TL.tagOfBinding = range,
		TL.nameOfBinding = name,
		TL.paramsOfBinding = params,
		TL.valueOfBinding = value
		})

-- `parseJavaScriptExprFromString` and `parseJavaScriptStatementsFromString`
-- try to interpret the given string as a JavaScript expression.

parseJavaScriptExprFromString :: String -> Either String (JS.Expression JS.SourcePos)
parseJavaScriptExprFromString string =
	case JS.parse JS.parseExpression "<string>" string' of
		Left err -> Left (show err)
		Right x -> return x
	where
		string' = dropWhile isSpace string

parseJavaScriptStatementsFromString :: String -> Either String [JS.Statement JS.SourcePos]
parseJavaScriptStatementsFromString string =
	case JS.parse (JS.parseStatement `Text.Parsec.sepBy` Text.Parsec.space) "<string>" string of
		Left err -> Left (show err)
		Right x -> return x
	where
		string' = dropWhile isSpace string

