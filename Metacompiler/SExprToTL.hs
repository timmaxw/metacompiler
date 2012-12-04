module Metacompiler.SExprToTL where

import Control.Monad (when, unless)
import Data.Char (isSpace)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Parser as JS
import qualified Language.ECMAScript3.Syntax as JS
import Metacompiler.SExpr
import Metacompiler.SExprToSL
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
			TL.nameOfDirective = name,
			TL.paramsOfDirective = params,
			TL.typeOfDirective = maybeType,
			TL.valueOfDirective = value
			}

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

parseTLDirectiveFromSExpr (List range (Cons (Atom r name) rest)) =
	Left ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)

parseTLDirectiveFromSExpr other =
	Left ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

-- `parseTLDirectiveFromSExpr` expects a `SExpr` that looks like
-- `(name :: type)`. It parses the name and type and returns them as a tuple.
-- It is used in `fun ... -> ...` types, `\ ... -> ...` terms, and at the top
-- of `let` and `js-expr` directives.

parseTLParameterFromSExpr :: SExpr -> Either String (String, TL.MetaType Range)
parseTLParameterFromSExpr (List r (Cons (Atom _ name) (Cons (Atom _ "::") ty))) = do
	ty' <-
		errorContext ("in type of parameter \"" ++ name ++ "\" at " ++ formatRange r) $
		parseTLMetaTypeFromSExprs ty
	return (name, ty')
parseTLParameterFromSExpr other =
	Left ("invalid parameter: expected \"(name :: type)\", got " ++ summarizeSExpr other ++
		" at " ++ formatRange (rangeOfSExpr other))

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
parseTLMetaTypeFromSExpr (Atom range "js-type") =
	return $ TL.MTJSType {
		TL.tagOfMetaType = range
		}
parseTLMetaTypeFromSExpr (List _ stuff) =
	parseTLMetaTypeFromSExprs stuff
parseTLMetaTypeFromSExpr other =
	Left ("invalid meta-type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaTypeFromSExprs` tries to interpret a list of S-expressions as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExprs :: SExprs -> Either String (TL.MetaType Range)
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ a) rest) | a `elem` ["js-term", "js-sl-term"] =
	errorContext ("in \"" ++ a ++ "\" meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		ty <- case rest of
			Nil p -> Left ("missing term-type at " ++ formatPoint p)
			Cons t (Nil _) -> parseTLMetaObjectFromSExpr t
			Cons _ _ -> Left ("term-type at " ++ formatRange (rangeOfSExprs rest) ++
				" must be enclosed in parentheses")
		return $ TL.MTJSTerm {
			TL.tagOfMetaType = rangeOfSExprs whole,
			TL.hasSLOfMetaType = a == "js-sl-term",
			TL.typeOfMetaType = ty
			}
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "fun") rest) =
	errorContext ("in \"fun\" meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		(unparsedParams, unparsedBody) <- breakOnAtom "->" rest
		params <- mapM parseTLParameterFromSExpr (sExprsToList unparsedParams)
		body <- parseTLMetaTypeFromSExprs unparsedBody
		return $ TL.MTFun {
			TL.tagOfMetaType = rangeOfSExprs whole,
			TL.paramsOfMetaType = params,
			TL.resultOfMetaType = body
			}
parseTLMetaTypeFromSExprs whole@(Cons x (Nil _)) =
	parseTLMetaTypeFromSExpr x
parseTLMetaTypeFromSExprs other =
	Left ("invalid meta-type " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseTLMetaObjectFromSExpr` tries to interpret a list of S-expressions as a
-- `TL.MetaObject`.

parseTLMetaObjectFromSExpr :: SExpr -> Either String (TL.MetaObject Range)
parseTLMetaObjectFromSExpr (Atom range x) =
	return $ TL.MOVar {
		TL.tagOfMetaObject = range,
		TL.varOfMetaObject = x
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
			TL.paramsOfMetaObject = args',
			TL.resultOfMetaObject = body
			}
parseTLMetaObjectFromSExprs whole@(Cons (Atom _ "js-expr") rest) =
	errorContext ("in \"js-expr\" at " ++ formatRange (rangeOfSExprs whole)) $ do
		(unparsedClauses, codeRange, unparsedCode) <- case sExprsInitAndLast rest of
			Just (unparsedClauses, Quoted codeRange unparsedCode) ->
				return (unparsedClauses, codeRange, unparsedCode)
			_ -> Left ("expected (js-expr <clauses...> \"<code>\")")
		clauses <- parseClausesFromSExprs [("type", False, False), ("spec", True, False), ("=", True, True)] unparsedClauses
		type_ <- let [(_, unparsedType)] = (M.!) clauses "type" in
			errorContext ("in type at " ++ formatRange (rangeOfSExprs unparsedType)) $
			parseTLMetaObjectFromSExprs unparsedType
		spec <- case (M.!) clauses "spec" of
			[(range, rest)] ->
				errorContext ("in (spec ...) clause at " ++ formatRange range) $ do
					spec <- parseSLTermFromSExprs rest
					return (Just spec)
			[] -> return Nothing
		subs <- sequence [do
			(name, value) <- case rest of
				Cons (Quoted _ name) value -> return (name, value)
				_ -> Left ("malformed (= ...) clause at " ++ formatRange range ++
					"; expected (= \"<name>\" <value>).")
			value' <- parseTLMetaObjectFromSExprs value
			return (name, value')
			| (range, rest) <- (M.!) clauses "="]
		code <-
			errorContext ("in code at " ++ formatRange codeRange) $
			parseJavaScriptExprFromString unparsedCode
		return $ TL.MOJSExpr {
			TL.tagOfMetaObject = rangeOfSExprs whole,
			TL.codeOfMetaObject = code,
			TL.typeOfMetaObject = type_,
			TL.specOfMetaObject = spec,
			TL.subsOfMetaObject = subs
			}
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
			TL.funOfMetaObject = f,
			TL.argOfMetaObject = a
			}
		) first' args')
parseTLMetaObjectFromSExprs other =
	Left ("invalid meta-object " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

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
	case JS.parse (JS.parseStatement `Text.Parsec.sepBy` Text.Parsec.space) of
		Left err -> Left (show err)
		Right x -> return x
	where
		string' = dropWhile isSpace string

