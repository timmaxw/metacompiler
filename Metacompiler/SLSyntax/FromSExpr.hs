module Metacompiler.SLSyntax.FromSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsFrom
import Metacompiler.SLSyntax.Types

-- `parseSLKindFromSExpr` tries to interpret an S-expression as a `Kind`. If
-- it doesn't work, then it returns `Left <error>`.

parseSLKindFromSExpr :: SExpr -> ErrorMonad (Kind Range)
parseSLKindFromSExpr (Atom r "*") =
	return (KindType r)
parseSLKindFromSExpr (List _ xs) =
	parseSLKindFromSExprs xs
parseSLKindFromSExpr other =
	fail ("invalid kind " ++ formatSExprForMessage other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLKindFromSExpr` tries to interpret a list of S-expressions as a
-- `Kind`.

parseSLKindFromSExprs :: SExprs -> ErrorMonad (Kind Range)
parseSLKindFromSExprs (Cons a (Nil _)) =
	parseSLKindFromSExpr a
parseSLKindFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- mapM parseSLKindFromSExpr (sExprsToList args)
	res <- parseSLKindFromSExprs rest2
	return (KindFun (rangeOfSExprs whole) args' res)
parseSLKindFromSExprs other =
	fail ("invalid kind " ++ formatSExprsForMessage other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTypeFromSExpr` tries to interpret a S-expression as a `Type`.

parseSLTypeFromSExpr :: SExpr -> ErrorMonad (Type Range)
parseSLTypeFromSExpr (Atom r a) | isValidTypeName a =
	return (TypeName r (NameOfType a))
parseSLTypeFromSExpr (List _ xs) =
	parseSLTypeFromSExprs xs
parseSLTypeFromSExpr other =
	fail ("invalid type " ++ formatSExprForMessage other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTypeFromSExpr` tries to interpret a list of S-expressions as a
-- `Type`.

parseSLTypeFromSExprs :: SExprs -> ErrorMonad (Type Range)
parseSLTypeFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- mapM parseSLTypeFromSExpr (sExprsToList args)
	res <- parseSLTypeFromSExprs rest2
	return (TypeFun (rangeOfSExprs whole) args' res)
parseSLTypeFromSExprs whole@(Cons (Atom _ "lazy") rest) = do
	inner <- parseSLTypeFromSExprs rest
	return (TypeLazy (rangeOfSExprs whole) inner)
parseSLTypeFromSExprs (Cons a (Nil _)) =
	parseSLTypeFromSExpr a
parseSLTypeFromSExprs whole@(Cons first args) = do
	first' <- parseSLTypeFromSExpr first
	args' <- sequence [do
		arg' <- parseSLTypeFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExpr first)
	return (foldl (\ f (a, endPoint) -> TypeApp (Range startPoint endPoint) f a) first' args')
parseSLTypeFromSExprs other =
	fail ("invalid type " ++ formatSExprsForMessage other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTermFromSExpr` tries to interpret a S-expression as a `Term`.

parseSLTermFromSExpr :: SExpr -> ErrorMonad (Term Range)
parseSLTermFromSExpr (List _ xs) =
	parseSLTermFromSExprs xs
parseSLTermFromSExpr other@(Atom r a) | isValidTermName a =
	return (TermName r (NameOfTerm a) [])
parseSLTermFromSExpr other =
	fail ("invalid term " ++ formatSExprForMessage other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTermFromSExpr` tries to interpret a list of S-expressions as a
-- `Term`.

parseSLTermFromSExprs :: SExprs -> ErrorMonad (Term Range)
parseSLTermFromSExprs whole@(Cons (Atom _ "\\") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- sequence [
		case arg of
			List _ (Cons (Atom _ argName) (Cons (Atom _ "::") argType)) -> do
				argType' <- parseSLTypeFromSExprs argType
				return (NameOfTerm argName, argType')
			_ -> fail ("malformed formal arg: " ++ formatSExprForMessage arg ++ "expected \"(name :: type)\"")
		| (arg, i) <- zip (sExprsToList args) [1..]]
	body <- parseSLTermFromSExprs rest2
	return (TermAbs (rangeOfSExprs whole) args' body)
parseSLTermFromSExprs whole@(Cons (Atom _ "wrap") rest) = do
	inner <- parseSLTermFromSExprs rest
	return (TermWrap (rangeOfSExprs whole) inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "unwrap") rest) = do
	inner <- parseSLTermFromSExprs rest
	return (TermUnwrap (rangeOfSExprs whole) inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "case") rest) = case rest of
	(Cons subject (Cons (Atom _ "of") rest2)) -> do
		subject' <- parseSLTermFromSExpr subject
		let parseClauses clauses = case clauses of
			(Cons (List _ pattern) (Cons (Atom _ "->") (Cons branch rest))) -> do
				(ctorAndTypeParams, fields) <- breakOnAtom "." pattern
				(ctor, typeParams) <- takeOne "constructor name" ctorAndTypeParams
				ctor' <- case ctor of
					Atom _ name -> return (NameOfTerm name)
					_ -> fail ("expected pattern of the form `(ctor types . fields)`, but the `ctor` was " ++
						formatSExprForMessage ctor ++ " instead of a name.")
				typeParams' <- sequence [parseSLTypeFromSExpr typeParam | typeParam <- sExprsToList typeParams]
				fields' <- sequence [
					case field of
						Atom _ f -> return (NameOfTerm f)
						_ -> fail ("expected pattern variable, got " ++ formatSExprForMessage field ++ " at " ++
							formatRange (rangeOfSExpr field))
					| field <- sExprsToList fields]
				branch' <- parseSLTermFromSExpr branch
				let clause' = (ctor', typeParams', fields', branch')
				otherClauses <- parseClauses rest
				return (clause':otherClauses)
			(Nil _) ->
				return []
			_ -> fail ("expected clause of the form `(ctor types . fields) -> expr`, instead got " ++
				formatSExprsForMessage clauses ++ " at " ++
				formatPoint (startOfRange (rangeOfSExprs clauses)))
		clauses' <- parseClauses rest2
		return (TermCase (rangeOfSExprs whole) subject' clauses')
parseSLTermFromSExprs (Cons a (Nil _)) =
	parseSLTermFromSExpr a
parseSLTermFromSExprs whole = do
	parts <- maybeBreakOnAtom "." whole
	(first', args) <- case parts of
		(Cons first args, Nothing) -> do
			first' <- parseSLTermFromSExpr first
			return (first', args)
		(nameAndTypeArgs@(Cons name typeArgs), Just args) -> do
			name' <- case name of
				Atom r a | isValidTermName a -> return (NameOfTerm a)
				_ -> fail ("at " ++ formatRange (rangeOfSExprs whole) ++ ": found expression with `.` in it, but `" ++
						formatSExprForMessage name ++ "` is not a valid name.")
			typeArgs' <- sequence [parseSLTypeFromSExpr typeArg | typeArg <- sExprsToList typeArgs]
			return (TermName (rangeOfSExprs nameAndTypeArgs) name' typeArgs', args) 
	args' <- sequence [do
		arg' <- parseSLTermFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExprs whole)
	return (foldl (\ f (a, endPoint) -> TermApp (Range startPoint endPoint) f a) first' args')

-- `parseSLDirFromSExpr` tries to interpret the given S-expression as a `Dir`.

parseSLDirFromSExpr :: SExpr -> ErrorMonad (Dir Range)
parseSLDirFromSExpr (List range (Cons (Atom _ "data") rest)) = do
	(nameAndParams, ctors) <- breakOnAtom "=" rest
	(name, params) <- takeOne "name" nameAndParams
	name' <- case name of
		Atom _ n -> return (NameOfType n)
		_ -> fail ("expected `(data <name> <params> = <ctors>)`")
	params' <- sequence [
		case thing of
			List _ (Atom _ paramName `Cons` Atom _ "::" `Cons` kind) -> do
				let paramName' = NameOfType paramName
				kind' <- parseSLKindFromSExprs kind
				return (paramName', kind')
			_ -> fail ("expected `(<name> :: <kind>)`")
		| thing <- sExprsToList params]
	ctors' <- sequence [
		case ctor of
			List range2 (Atom _ ctorName `Cons` fields) -> do
				let ctorName' = NameOfTerm ctorName
				fields' <- mapM parseSLTypeFromSExpr (sExprsToList fields)
				return (ctorName', fields')
			_ -> fail ("cannot parse constructor at " ++ formatRange (rangeOfSExpr ctor))
		| ctor <- sExprsToList ctors]
	return $ DirData {
		tagOfDir = range,
		nameOfDirData = name',
		typeParamsOfDirData = params',
		ctorsOfDirData = ctors'
		}
parseSLDirFromSExpr (List range (Cons (Atom _ "let") rest)) = do
	let msgPrefix = "in `let` directive at " ++ formatRange range ++ ":"
	(nameAndAllParamsAndType, value) <- errorContext msgPrefix $ breakOnAtom "=" rest
	(nameAndAllParams, type_) <- errorContext msgPrefix $ breakOnAtom "::" nameAndAllParamsAndType
	(nameAndTypeParams, termParams) <- errorContext msgPrefix $ breakOnAtom "." nameAndAllParams
	(name, typeParams) <- errorContext msgPrefix $ takeOne "name" nameAndTypeParams
	name' <- case name of
		Atom _ n -> return (NameOfTerm n)
		_ -> fail ("expected atom as name")
	typeParams' <- sequence [
		case thing of
			List _ (Atom _ paramName `Cons` Atom _ "::" `Cons` kind) -> do
				let paramName' = NameOfType paramName
				kind' <- parseSLKindFromSExprs kind
				return (paramName', kind')
			_ -> fail ("expected `(<name> :: <kind>)`")
		| thing <- sExprsToList typeParams]
	termParams' <- sequence [
		case thing of
			List _ (Atom _ paramName `Cons` Atom _ "::" `Cons` type_) -> do
				let paramName' = NameOfTerm paramName
				type_' <- parseSLTypeFromSExprs type_
				return (paramName', type_')
			_ -> fail ("expected `(<name> :: <type>)`")
		| thing <- sExprsToList termParams]
	type_' <- parseSLTypeFromSExprs type_
	value' <- parseSLTermFromSExprs value
	return $ DirLet {
		tagOfDir = range,
		nameOfDirLet = name',
		typeParamsOfDirLet = typeParams',
		termParamsOfDirLet = termParams',
		typeOfDirLet = type_',
		valueOfDirLet = value'
		}
parseSLDirFromSExpr (List range (Cons (Atom r name) _)) =
	fail ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)
parseSLDirFromSExpr other =
	fail ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

