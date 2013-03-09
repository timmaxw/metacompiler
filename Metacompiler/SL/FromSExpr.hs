module Metacompiler.SL.FromSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsFrom
import Metacompiler.SL.Syntax as SL

-- `parseSLKindFromSExpr` tries to interpret an S-expression as a `SL.Kind`. If
-- it doesn't work, then it fails.

parseSLKindFromSExpr :: SExpr -> BacktraceMonad (SL.Kind Location)
parseSLKindFromSExpr (Atom r "*") = do
	loc <- getLocation r
	return (SL.KindType loc)
parseSLKindFromSExpr (List _ xs) =
	parseSLKindFromSExprs xs
parseSLKindFromSExpr other =
	fail ("invalid kind " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLKindFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Kind`.

parseSLKindFromSExprs :: SExprs -> BacktraceMonad (SL.Kind Location)
parseSLKindFromSExprs (Cons a (Nil _)) =
	parseSLKindFromSExpr a
parseSLKindFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <-
		frameBacktrace ("in arg-kind of " ++ summarizeSExprs whole ++ " starting at " ++ formatRange (rangeOfSExprs whole)) $
		mapM parseSLKindFromSExpr (sExprsToList args)
	res <-
		frameBacktrace ("in result-kind of " ++ summarizeSExprs whole ++ " starting at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLKindFromSExprs rest2
	loc <- getLocation (rangeOfSExprs whole)
	return (KindFun loc args' res)
parseSLKindFromSExprs other =
	fail ("invalid kind " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTypeFromSExpr` tries to interpret a S-expression as a `SL.Type`.

parseSLTypeFromSExpr :: SExpr -> BacktraceMonad (SL.Type Location)
parseSLTypeFromSExpr (Atom r a) | SL.isValidTypeName a = do
	l <- getLocation r
	return (SL.TypeName loc (SL.NameOfType a))
parseSLTypeFromSExpr (List _ xs) =
	parseSLTypeFromSExprs xs
parseSLTypeFromSExpr other =
	fail ("invalid type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTypeFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Type`.

parseSLTypeFromSExprs :: SExprs -> BacktraceMonad (SL.Type Location)
parseSLTypeFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <-
		frameBacktrace ("in arg-type of " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		mapM parseSLTypeFromSExpr (sExprsToList args)
	res <-
		frameBacktrace ("in result-type of " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTypeFromSExprs rest2
	loc <- getLocation (rangeOfSExprs whole)
	return (TypeFun loc args' res)
parseSLTypeFromSExprs whole@(Cons (Atom _ "lazy") rest) = do
	inner <-
		frameBacktrace ("in inner type of \"lazy ...\" at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTypeFromSExprs rest
	loc <- getLocation (rangeOfSExprs whole)
	return (TypeLazy loc inner)
parseSLTypeFromSExprs (Cons a (Nil _)) =
	parseSLTypeFromSExpr a
parseSLTypeFromSExprs whole@(Cons first args) = do
	first' <-
		frameBacktrace ("in type being specialized in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTypeFromSExpr first
	args' <- sequence [do
		arg' <-
			frameBacktrace ("in argument #" ++ show i ++ " in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
			parseSLTypeFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExpr first)
	bt <- getBacktrace
	return (foldl
		(\ f (a, endPoint) -> TypeApp (Location (Range startPoint endPoint) bt) f a)
		first'
		args')
parseSLTypeFromSExprs other =
	fail ("invalid type " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTermFromSExpr` tries to interpret a S-expression as a `SL.Term`.

parseSLTermFromSExpr :: SExpr -> Either String (SL.Term Range)
parseSLTermFromSExpr (Atom r a) | SL.isValidTermName a =
	return (SL.TermName r (SL.NameOfTerm a) [])   -- TODO: type parameters
parseSLTermFromSExpr (List _ xs) =
	parseSLTermFromSExprs xs
parseSLTermFromSExpr other =
	Left ("invalid term " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTermFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Term`.

parseSLTermFromSExprs :: SExprs -> BacktraceMonad (SL.Term Location)
parseSLTermFromSExprs whole@(Cons (Atom _ "\\") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- sequence [
		frameBacktrace ("in formal arg #" ++ show i ++ " of " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		case arg of
			List _ (Cons (Atom _ argName) (Cons (Atom _ "::") argType)) -> do
				argType' <-
					errorContext ("in argument's type") $
					parseSLTypeFromSExprs argType
				return (SL.NameOfTerm argName, argType')
			_ -> fail ("malformed formal arg: " ++ summarizeSExpr arg ++ "expected \"(name :: type)\"")
		| (arg, i) <- zip (sExprsToList args) [1..]]
	body <-
		frameBacktrace ("in body of " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTermFromSExprs rest2
	loc <- getLocation (rangeOfSExprs whole)
	return (TermAbs loc args' body)
parseSLTermFromSExprs whole@(Cons (Atom _ "wrap") rest) = do
	inner <-
		frameBacktrace ("in inner value of \"wrap ...\" at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTermFromSExprs rest
	loc <- getLocation (rangeOfSExprs whole)
	return (TermWrap loc inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "unwrap") rest) = do
	inner <-
		frameBacktrace ("in inner value of \"unwrap ...\" at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTermFromSExprs rest
	loc <- getLocation (rangeOfSExprs whole)
	return (TermWrap loc inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "case") rest) = case rest of
	(Cons subject (Cons (Atom _ "of") rest2)) -> do
		subject' <-
			errorContext ("in subject of \"case ...\" at " ++ formatRange (rangeOfSExprs whole)) $
			parseSLTermFromSExpr subject
		let parseClauses clauses = case clauses of
			(Cons pattern (Cons (Atom _ "->") (Cons branch rest))) -> do
				let clause = Cons pattern $ Cons (Atom undefined "->") $ Cons branch $ Nil undefined
				clause <- errorContext ("in clause " ++ summarizeSExprs clause ++
						" at " ++ formatRange (rangeOfSExprs clause)) $ do
					(ctor, vars) <- case pattern of
						Atom _ name ->
							return (SL.NameOfTerm name, [])
						List _ (Cons (Atom _ name) vars) -> do
							vars' <- sequence [
								case var of
									Atom _ v -> return (SL.NameOfTerm v)
									_ -> Left ("expected pattern variable, got " ++
										summarizeSExpr var ++ " at " ++
										formatRange (rangeOfSExpr var))
								| var <- sExprsToList vars]
							return (SL.NameOfTerm name, vars')
						_ -> Left ("expected pattern, of the form \"ctor\" or \"(ctor var1 var2 \
							\...)\", instead got " ++ summarizeSExpr pattern ++ " at " ++
							formatRange (rangeOfSExpr pattern))
					branch' <- parseSLTermFromSExpr branch
					return (ctor, [], vars, branch')
				otherClauses <- parseClauses rest
				return (clause:otherClauses)
			(Nil _) ->
				return []
			_ -> Left ("expected clause of the form \"pattern -> expr\", instead got " ++
				summarizeSExprs clauses ++ " at " ++ formatPoint (startOfRange (rangeOfSExprs clauses)))
		clauses' <-
			errorContext ("in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
			parseClauses rest2
		return (TermCase (rangeOfSExprs whole) subject' clauses')
parseSLTermFromSExprs (Cons a (Nil _)) =
	parseSLTermFromSExpr a
parseSLTermFromSExprs whole@(Cons first args) = do
	first' <-
		errorContext ("in function being called in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
		parseSLTermFromSExpr first
	args' <- sequence [do
		arg' <-
			errorContext ("in argument #" ++ show i ++ " in " ++ summarizeSExprs whole ++ " at " ++ formatRange (rangeOfSExprs whole)) $
			parseSLTermFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExpr first)
	return (foldl (\ f (a, endPoint) -> TermApp (Range startPoint endPoint) f a) first' args')
parseSLTermFromSExprs other =
	Left ("invalid term " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLDirFromSExpr` tries to interpret the given S-expression as a `SL.Dir`.

parseSLDirFromSExpr :: SExpr -> Either String (SL.Dir Range)
parseSLDirFromSExpr (List range (Cons (Atom _ "data") rest)) =
	case rest of
		Atom _ name `Cons` Atom _ "=" `Cons` ctors -> do
			let name' = SL.NameOfType name
			ctors' <- sequence [
				case ctor of
					List range2 (Atom _ ctorName `Cons` fields) -> do
						let ctorName' = SL.NameOfTerm ctorName
						fields' <- mapM parseSLTypeFromSExpr (sExprsToList fields)
						return (ctorName', fields')
					_ -> Left ("cannot parse constructor at " ++ formatRange (rangeOfSExpr ctor))
				| ctor <- sExprsToList ctors]
			return $ SL.DirData {
				SL.tagOfDir = range,
				SL.nameOfDirData = name',
				SL.typeParamsOfDirData = [],
				SL.ctorsOfDirData = ctors'
				}
		_ -> Left ("expected `(data <name> = <ctors>)`")
parseSLDirFromSExpr (List range (Cons (Atom _ "let") rest)) = do
	(nameAndTermParamsAndType, value) <- breakOnAtom "=" rest
	(nameAndTermParams, type_) <- breakOnAtom "::" nameAndTermParamsAndType
	(name, termParams) <- takeOne "name" nameAndTermParams
	name' <- case name of
		Atom _ n -> return (SL.NameOfTerm n)
		_ -> Left ("expected atom as name")
	termParams' <- sequence [
		case thing of
			List _ (Atom _ paramName `Cons` Atom _ "::" `Cons` type_) -> do
				let paramName' = SL.NameOfTerm paramName
				type_' <- parseSLTypeFromSExprs type_
				return (paramName', type_')
			_ -> Left ("expected `(<name> :: <type>)`")
		| thing <- sExprsToList termParams]
	type_' <- parseSLTypeFromSExprs type_
	value' <- parseSLTermFromSExprs value
	return $ SL.DirLet {
		SL.tagOfDir = range,
		SL.nameOfDirLet = name',
		SL.typeParamsOfDirLet = [],
		SL.termParamsOfDirLet = termParams',
		SL.typeOfDirLet = type_',
		SL.valueOfDirLet = value'
		}
parseSLDirFromSExpr (List range (Cons (Atom r name) _)) =
	Left ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)
parseSLDirFromSExpr other =
	Left ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

