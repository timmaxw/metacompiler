module Metacompiler.SExprToTL where

import Control.Monad (when, unless)
import qualified Data.Map as M
import Language.JavaScript.Parser
import Metacompiler.SExpr
import Metacompiler.SExprToSL
import qualified Metacompiler.SL as SL
import qualified Metacompiler.TL as TL

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
		clauses <- parseClausesFromSExprs ["spec"] [] rest3
		spec <- let (range, spec) = (M.!) clauses "spec" in
			errorContext ("in \"spec\" clause at " ++ formatRange range) $
			parseSLTypeFromSExprs spec
		return $ TL.DJSRepr {
			TL.tagOfDirective = range,
			TL.nameOfDirective = name,
			TL.paramsOfDirective = params,
			TL.specOfDirective = spec
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
-- `(keyword ...)`. The first two parameters specify the allowed keywords.
-- Mandatory clauses must appear exactly once; optional clauses must appear at
-- most once. It returns a map containing the clauses, without parsing their
-- contents.

parseClausesFromSExprs :: [String] -> [String] -> SExprs -> Either String (M.Map String (Range, SExprs))
parseClausesFromSExprs mandatoryClauses optionalClauses seq = do
	clauses <- parseClauses' seq
	sequence [
		unless (name `M.member` clauses) $
			Left ("missing mandatory clause \"" ++ name ++ "\" at " ++ formatRange (rangeOfSExprs seq))
		| name <- mandatoryClauses]
	return clauses
	where
		parseClauses' :: SExprs -> Either String (M.Map String (Range, SExprs))
		parseClauses' (Nil _) = return M.empty
		parseClauses' (Cons c rest) = do
			(name, value) <- case c of
				List _ (Cons (Atom _ name) value) -> return (name, value)
				_ -> Left ("invalid clause " ++ summarizeSExpr c ++ " at " ++ formatRange (rangeOfSExpr c))
			unless (name `elem` optionalClauses || name `elem` mandatoryClauses) $
				Left ("invalid clause type \"" ++ name ++ "\"")
			rest' <- parseClauses' rest
			case M.lookup name rest' of
				Nothing ->
					return ()
				Just (range, _) ->
					Left ("duplicated clause \"" ++ name ++ "\" at " ++
						formatRange (rangeOfSExpr c) ++ " and " ++ formatRange range)
			return (M.insert name (rangeOfSExpr c, value) rest')

-- `parseTLMetaTypeFromSExpr` tries to interpret an S-expression as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExpr :: SExpr -> Either String (TL.MetaType Range)
parseTLMetaTypeFromSExpr (Atom range "type") =
	return $ TL.MTType {
		TL.tagOfMetaType = range
		}
parseTLMetaTypeFromSExpr (List _ stuff) =
	parseTLMetaTypeFromSExprs stuff
parseTLMetaTypeFromSExpr other =
	Left ("invalid meta-type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseTLMetaTypeFromSExprs` tries to interpret a list of S-expressions as a
-- `TL.MetaType`.

parseTLMetaTypeFromSExprs :: SExprs -> Either String (TL.MetaType Range)
parseTLMetaTypeFromSExprs whole@(Cons (Atom _ "term") rest) =
	errorContext ("in \"term\" meta-type at " ++ formatRange (rangeOfSExprs whole)) $ do
		ty <- case rest of
			Nil p -> Left ("missing term-type at " ++ formatPoint p)
			Cons t (Nil _) -> parseTLMetaObjectFromSExpr t
			Cons _ _ -> Left ("term-type at " ++ formatRange (rangeOfSExprs rest) ++
				" must be enclosed in parentheses")
		return $ TL.MTTerm {
			TL.tagOfMetaType = rangeOfSExprs whole,
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
parseTLMetaObjectFromSExpr (Quoted cr code) = do
	code' <- Language.JavaScript.Parser.parse code ("embedded JS substitution at " ++ formatRange cr)
	return TL.MOJSSubstitution {
		TL.tagOfMetaObject = cr,
		TL.jsSubstitutionOfMetaObject = code'
		}

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
		clauses <- parseClausesFromSExprs ["type", "spec", "impl"] [] rest
		ty <- let (range, rest) = (M.!) clauses "type" in
			errorContext ("in \"type\" clause at " ++ formatRange range) $
			parseTLMetaObjectFromSExprs rest
		spec <- let (range, rest) = (M.!) clauses "spec" in
			errorContext ("in \"spec\" clause at " ++ formatRange range) $
			parseSLTermFromSExprs rest
		impl <- let (range, rest) = (M.!) clauses "impl" in
			errorContext ("in \"impl\" clause at " ++ formatRange range) $
			parseTLJavascriptBlockFromSExprs rest
		return $ TL.MOJSExpr {
			TL.tagOfMetaObject = rangeOfSExprs whole,
			TL.typeOfMetaObject = ty,
			TL.specOfMetaObject = spec,
			TL.implOfMetaObject = impl
			}
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

-- `parseTLJavascriptBlockFromSExprs` tries to interpret a list of
-- S-expressions as a `TL.JavascriptBlock`; that is, a quoted Javascript code
-- block followed by a series of variable substitutions.

parseTLJavascriptBlockFromSExprs :: SExprs -> Either String (TL.JavascriptBlock Range)
parseTLJavascriptBlockFromSExprs whole@(Cons (Quoted cr code) vars) = do
	code' <- Language.JavaScript.Parser.parse code ("embedded js-expr block at " ++ formatRange cr)
	let
		parseVar :: SExpr -> Either String (String, TL.JavascriptBlockVar Range)
		parseVar (List _ (Cons (Atom _ "set") (Cons (Quoted _ var) value))) = do
			value' <- parseTLMetaObjectFromSExprs value
			return (var, TL.JBVSet { TL.valueOfJavascriptBlockVar = value' })
		parseVar (List _ (Cons (Atom _ "free") (Cons (Quoted _ var) (Nil _)))) =
			return (var, TL.JBVFree)
		parseVar other =
			Left ("invalid var specification " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))
	vars' <- mapM parseVar (sExprsToList vars)
	return $ TL.JavascriptBlock {
		TL.tagOfJavascriptBlock = rangeOfSExprs whole,
		TL.codeOfJavascriptBlock = code',
		TL.varsOfJavascriptBlock = vars'
		}

	
