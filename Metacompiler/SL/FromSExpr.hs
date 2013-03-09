module Metacompiler.SL.FromSExpr where

import Metacompiler.Error
import Metacompiler.SExpr.Format
import Metacompiler.SExpr.Types
import Metacompiler.SExpr.UtilsFrom
import Metacompiler.SL.Syntax as SL

-- `parseSLKindFromSExpr` tries to interpret an S-expression as a `SL.Kind`. If
-- it doesn't work, then it returns `Left <error>`.

parseSLKindFromSExpr :: SExpr -> Either String (SL.Kind Range)
parseSLKindFromSExpr (Atom r "*") =
	return (SL.KindType r)
parseSLKindFromSExpr (List _ xs) =
	parseSLKindFromSExprs xs
parseSLKindFromSExpr other =
	fail ("invalid kind " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLKindFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Kind`.

parseSLKindFromSExprs :: SExprs -> Either String (SL.Kind Range)
parseSLKindFromSExprs (Cons a (Nil _)) =
	parseSLKindFromSExpr a
parseSLKindFromSExprs whole@(Cons (Atom _ "fun") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- mapM parseSLKindFromSExpr (sExprsToList args)
	res <- parseSLKindFromSExprs rest2
	return (KindFun (rangeOfSExprs whole) args' res)
parseSLKindFromSExprs other =
	fail ("invalid kind " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTypeFromSExpr` tries to interpret a S-expression as a `SL.Type`.

parseSLTypeFromSExpr :: SExpr -> Either String (SL.Type Range)
parseSLTypeFromSExpr (Atom r a) | SL.isValidTypeName a =
	return (SL.TypeName r (SL.NameOfType a))
parseSLTypeFromSExpr (List _ xs) =
	parseSLTypeFromSExprs xs
parseSLTypeFromSExpr other =
	fail ("invalid type " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTypeFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Type`.

parseSLTypeFromSExprs :: SExprs -> Either String (SL.Type Range)
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
	fail ("invalid type " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

-- `parseSLTermFromSExpr` tries to interpret a S-expression as a `SL.Term`.

parseSLTermFromSExpr :: SExpr -> Either String (SL.Term Range)
parseSLTermFromSExpr (Atom r a) | SL.isValidTermName a =
	return (SL.TermName r (SL.NameOfTerm a) [])   -- TODO: type parameters
parseSLTermFromSExpr (List _ xs) =
	parseSLTermFromSExprs xs
parseSLTermFromSExpr other =
	fail ("invalid term " ++ summarizeSExpr other ++ " at " ++ formatRange (rangeOfSExpr other))

-- `parseSLTermFromSExpr` tries to interpret a list of S-expressions as a
-- `SL.Term`.

parseSLTermFromSExprs :: SExprs -> Either String (SL.Term Range)
parseSLTermFromSExprs whole@(Cons (Atom _ "\\") rest) = do
	(args, rest2) <- breakOnAtom "->" rest
	args' <- sequence [
		case arg of
			List _ (Cons (Atom _ argName) (Cons (Atom _ "::") argType)) -> do
				argType' <- parseSLTypeFromSExprs argType
				return (SL.NameOfTerm argName, argType')
			_ -> fail ("malformed formal arg: " ++ summarizeSExpr arg ++ "expected \"(name :: type)\"")
		| (arg, i) <- zip (sExprsToList args) [1..]]
	body <- parseSLTermFromSExprs rest2
	return (TermAbs (rangeOfSExprs whole) args' body)
parseSLTermFromSExprs whole@(Cons (Atom _ "wrap") rest) = do
	inner <- parseSLTermFromSExprs rest
	return (TermWrap (rangeOfSExprs whole) inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "unwrap") rest) = do
	inner <- parseSLTermFromSExprs rest
	return (TermWrap (rangeOfSExprs whole) inner)
parseSLTermFromSExprs whole@(Cons (Atom _ "case") rest) = case rest of
	(Cons subject (Cons (Atom _ "of") rest2)) -> do
		subject' <- parseSLTermFromSExpr subject
		let parseClauses clauses = case clauses of
			(Cons pattern (Cons (Atom _ "->") (Cons branch rest))) -> do
				let clause = Cons pattern $ Cons (Atom undefined "->") $ Cons branch $ Nil undefined
				(ctor, vars) <- case pattern of
					Atom _ name ->
						return (SL.NameOfTerm name, [])
					List _ (Cons (Atom _ name) vars) -> do
						vars' <- sequence [
							case var of
								Atom _ v -> return (SL.NameOfTerm v)
								_ -> fail ("expected pattern variable, got " ++
									summarizeSExpr var ++ " at " ++
									formatRange (rangeOfSExpr var))
							| var <- sExprsToList vars]
						return (SL.NameOfTerm name, vars')
					_ -> fail ("expected pattern, of the form \"ctor\" or \"(ctor var1 var2 \
						\...)\", instead got " ++ summarizeSExpr pattern ++ " at " ++
						formatRange (rangeOfSExpr pattern))
				branch' <- parseSLTermFromSExpr branch
				let clause' = (ctor, [], vars, branch')
				otherClauses <- parseClauses rest
				return (clause':otherClauses)
			(Nil _) ->
				return []
			_ -> fail ("expected clause of the form \"pattern -> expr\", instead got " ++
				summarizeSExprs clauses ++ " at " ++ formatPoint (startOfRange (rangeOfSExprs clauses)))
		clauses' <- parseClauses rest2
		return (TermCase (rangeOfSExprs whole) subject' clauses')
parseSLTermFromSExprs (Cons a (Nil _)) =
	parseSLTermFromSExpr a
parseSLTermFromSExprs whole@(Cons first args) = do
	first' <- parseSLTermFromSExpr first
	args' <- sequence [do
		arg' <- parseSLTermFromSExpr arg
		return (arg', endOfRange (rangeOfSExpr arg))
		| (arg, i) <- zip (sExprsToList args) [1..]]
	let startPoint = startOfRange (rangeOfSExpr first)
	return (foldl (\ f (a, endPoint) -> TermApp (Range startPoint endPoint) f a) first' args')
parseSLTermFromSExprs other =
	fail ("invalid term " ++ summarizeSExprs other ++ " at " ++ formatRange (rangeOfSExprs other))

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
					_ -> fail ("cannot parse constructor at " ++ formatRange (rangeOfSExpr ctor))
				| ctor <- sExprsToList ctors]
			return $ SL.DirData {
				SL.tagOfDir = range,
				SL.nameOfDirData = name',
				SL.typeParamsOfDirData = [],
				SL.ctorsOfDirData = ctors'
				}
		_ -> fail ("expected `(data <name> = <ctors>)`")
parseSLDirFromSExpr (List range (Cons (Atom _ "let") rest)) = do
	(nameAndTermParamsAndType, value) <- breakOnAtom "=" rest
	(nameAndTermParams, type_) <- breakOnAtom "::" nameAndTermParamsAndType
	(name, termParams) <- takeOne "name" nameAndTermParams
	name' <- case name of
		Atom _ n -> return (SL.NameOfTerm n)
		_ -> fail ("expected atom as name")
	termParams' <- sequence [
		case thing of
			List _ (Atom _ paramName `Cons` Atom _ "::" `Cons` type_) -> do
				let paramName' = SL.NameOfTerm paramName
				type_' <- parseSLTypeFromSExprs type_
				return (paramName', type_')
			_ -> fail ("expected `(<name> :: <type>)`")
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
	fail ("invalid directive type \"" ++ name ++ "\" at " ++ formatRange r)
parseSLDirFromSExpr other =
	fail ("invalid directive at top level at " ++ formatRange (rangeOfSExpr other))

