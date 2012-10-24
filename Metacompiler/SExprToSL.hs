module Metacompiler.SExprToSL where

import Metacompiler.ParseUtils
import Metacompiler.SExpr
import Metacompiler.SL as SL

parseSLKindFromSExpr :: SExpr Range -> Either String (SL.Kind Range)
parseSLKindFromSExpr (Atom r "*") =
	return (SL.KindType r)
parseSLKindFromSExpr (List _ xs) =
	parseSLKindFromSExprs xs
parseSLKindFromSExpr other =
	Left ("invalid kind " ++ summarizeSExpr other ++ " at " ++ formatRange (sExprRange other))

parseSLKindFromSExprs :: [SExpr Range] -> Either String (SL.Kind Range)
parseSLKindFromSExprs [a] =
	parseSLKindFromSExpr a
parseSLKindFromSExprs whole@(Atom _ "fun":rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <-
		errorContext ("in arg-kind of " ++ summarizeSExprs whole ++ " starting at " ++ formatRange (sExprsRange whole)) $
		mapM parseSLKindFromSExpr args
	res <-
		errorContext ("in result-kind of " ++ summarizeSExprs whole ++ " starting at " ++ formatRange (sExprsRange whole)) $
		parseSLKindFromSExprs rest2
	return (KindFun (sExprsRange whole) args' res)
parseSLKindFromSExprs other =
	Left ("invalid kind " ++ summarizeSExprs other ++ " at " ++ formatRange (sExprsRange other))

parseSLTypeFromSExpr :: SExpr Range -> Either String (SL.Type Range)
parseSLTypeFromSExpr (Atom r a) =
	return (SL.TypeName r a)
parseSLTypeFromSExpr (List _ xs) =
	parseSLTypeFromSExprs xs
parseSLTypeFromSExpr other =
	Left ("invalid type " ++ summarizeSExpr other ++ " at " ++ formatRange (sExprRange other))

parseSLTypeFromSExprs :: [SExpr Range] -> Either String (SL.Type Range)
parseSLTypeFromSExprs [a] =
	parseSLTypeFromSExpr a
parseSLTypeFromSExprs whole@(Atom _ "fun":rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <-
		errorContext ("in arg-type of " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		mapM parseSLTypeFromSExpr args
	res <-
		errorContext ("in result-type of " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		parseSLTypeFromSExprs rest2
	return (TypeFun (sExprsRange whole) args' res)
parseSLTypeFromSExprs whole@(Atom _ "lazy":rest) = do
	inner <-
		errorContext ("in inner type of \"lazy ...\" at " ++ formatRange (sExprsRange whole)) $
		parseSLTypeFromSExprs rest
	return (TypeLazy (sExprsRange whole) inner)
parseSLTypeFromSExprs whole@(first:args) = do
	first' <-
		errorContext ("in type being specialized in " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		parseSLTypeFromSExpr first
	args' <- sequence [do
		arg' <-
			errorContext ("in argument #" ++ show i ++ " in " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
			parseSLTypeFromSExpr arg
		return (arg', rangeEnd (sExprRange arg))
		| (arg, i) <- zip args [1..]]
	let startPoint = rangeStart (sExprRange first)
	return (foldl (\ f (a, endPoint) -> TypeApp (Range startPoint endPoint) f a) first' args')
parseSLTypeFromSExprs other =
	Left ("invalid type " ++ summarizeSExprs other ++ " at " ++ formatRange (sExprsRange other))

parseSLTermFromSExpr :: SExpr Range -> Either String (SL.Term Range)
parseSLTermFromSExpr (Atom r a) =
	return (SL.TermName r a [])
parseSLTermFromSExpr (List _ xs) =
	parseSLTermFromSExprs xs
parseSLTermFromSExpr other =
	Left ("invalid term " ++ summarizeSExpr other ++ " at " ++ formatRange (sExprRange other))

parseSLTermFromSExprs :: [SExpr Range] -> Either String (SL.Term Range)
parseSLTermFromSExprs [a] =
	parseSLTermFromSExpr a
parseSLTermFromSExprs whole@(Atom _ "\\":rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- sequence [
		errorContext ("in formal arg #d of " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		case arg of
			List _ (Atom _ argName:Atom _ "::":argType) -> do
				argType' <-
					errorContext ("in argument's type") $
					parseSLTypeFromSExprs argType
				return (argName, argType')
			_ -> Left ("malformed formal arg: " ++ summarizeSExpr arg ++ "expected \"(name :: type)\"")
		| (arg, i) <- zip args [1..]]
	body <-
		errorContext ("in body of " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		parseSLTermFromSExprs rest2
	return (TermAbs (sExprsRange whole) args' body)
parseSLTermFromSExprs whole@(Atom _ "wrap":rest) = do
	inner <-
		errorContext ("in inner value of \"wrap ...\" at " ++ formatRange (sExprsRange whole)) $
		parseSLTermFromSExprs rest
	return (TermWrap (sExprsRange whole) inner)
parseSLTermFromSExprs whole@(Atom _ "unwrap":rest) = do
	inner <-
		errorContext ("in inner value of \"unwrap ...\" at " ++ formatRange (sExprsRange whole)) $
		parseSLTermFromSExprs rest
	return (TermWrap (sExprsRange whole) inner)
parseSLTermFromSExprs whole@(Atom _ "case":rest) = case rest of
	(subject:Atom _ "of":rest2) -> do
		subject' <-
			errorContext ("in subject of \"case ...\" at " ++ formatRange (sExprsRange whole)) $
			parseSLTermFromSExpr subject
		let parseClauses clauses = case clauses of
			(pattern:Atom _ "->":branch:rest) ->
				errorContext ("in clause " ++ summarizeSExprs (take 3 clauses) ++
						" at " ++ formatRange (sExprsRange (take 3 clauses))) $ do
					(ctor, vars) <- case pattern of
						Atom _ name ->
							return (name, [])
						List _ (Atom _ name:vars) -> do
							vars' <- sequence [
								case var of
									Atom _ v -> return v
									_ -> Left ("expected pattern variable, got " ++
										summarizeSExpr var ++ " at " ++
										formatRange (sExprRange var))
								| var <- vars]
							return (name, vars')
						_ -> Left ("expected pattern, of the form \"ctor\" or \"(ctor var1 var2 \
							\...)\", instead got " ++ summarizeSExpr pattern ++ " at " ++
							formatRange (sExprRange pattern))
					branch' <- parseSLTermFromSExpr branch
					otherClauses <- parseClauses rest
					return ((ctor, vars, branch'):otherClauses)
			_ -> Left ("expected clause of the form \"pattern -> expr\", instead got " ++
				summarizeSExprs clauses ++ " at " ++ formatPoint (rangeStart (sExprsRange clauses)))
		clauses' <-
			errorContext ("in " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
			parseClauses rest2
		return (TermCase (sExprsRange whole) subject' clauses')
parseSLTermFromSExprs whole@(first:args) = do
	first' <-
		errorContext ("in function being called in " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
		parseSLTermFromSExpr first
	args' <- sequence [do
		arg' <-
			errorContext ("in argument #" ++ show i ++ " in " ++ summarizeSExprs whole ++ " at " ++ formatRange (sExprsRange whole)) $
			parseSLTermFromSExpr arg
		return (arg', rangeEnd (sExprRange arg))
		| (arg, i) <- zip args [1..]]
	let startPoint = rangeStart (sExprRange first)
	return (foldl (\ f (a, endPoint) -> TermApp (Range startPoint endPoint) f a) first' args')
parseSLTermFromSExprs other =
	Left ("invalid term " ++ summarizeSExprs other ++ " at " ++ formatRange (sExprsRange other))

