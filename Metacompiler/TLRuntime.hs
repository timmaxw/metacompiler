module Metacompiler.TLRuntime where

import Control.Monad (liftM)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import Metacompiler.GenSym
import qualified Metacompiler.SLSyntax as SL
import Metacompiler.TLSyntax

-- A `NormedMetaType' is the result of de-sugaring a type and evaluating any
-- variables in the types of `js-term ...` meta-types.

data NormedMetaType

	= NMTFun NormedMetaType NormedMetaType

	| NMTJSType

	-- This is `Nothing` if the term comes from a bare string, and `Just ...`
	-- where `...` is the type of the term otherwise. This `JSType` is the only
	-- is the only context where a `JSTypePlaceholder` should ever occur.
	| NMTJSTerm (Maybe JSType)

	deriving (Show, Eq)

tryToCastNormedMetaType :: NormedMetaType -> NormedMetaType -> Either String ()
tryToCastNormedMetaType a b
	| canCast a b = return ()
	| otherwise = Left ("expected a value of type " ++ formatNormedMetaType b ++
		", but instead got a value of type " ++ formatNormedMetaType a)
	where
		canCast :: NormedMetaType -> NormedMetaType -> Bool
		canCast (NMTFun arg1 ret1) (NMTFun arg2 ret2) =
			(arg1 == ret1) && (arg2 == ret2)
		canCast (NMTJSType) (NMTJSType) = True
		canCast (NMTJSTerm Nothing) (NMTJSTerm _) = True
		canCast (NMTJSTerm (Just type1)) (NMTJSTerm (Just type2)) =
			type1 == type2
		canCast _ _ = False

computeMetaType :: M.Map String NormedMetaType ->  MetaObject Range -> Either String NormedMetaType
computeMetaType vars (MOApp tag fun arg) = do
	funType <-
		errorContext ("in function of application at " ++ formatRange tag) $
		computeMetaType vars fun
	argType <-
		errorContext ("in argument of application at " ++ formatRange tag) $
		computeMetaType vars arg
	case funType of
		NMTFun argType' retType -> do
			errorContext ("for argument of application at " ++
					formatRange tag) $
				tryToCastNormedMetaType argType argType'
			return res
		_ -> Left ("in application at " ++ formatRange tag ++ ", the thing to \
			\be called has type " ++ formatNormedMetaType funType ++ ", which \
			\is not the type of a function.")
computeMetaType vars (MOAbs tag params result) = do
	let
		processParams :: M.Map String NormedMetaType
		              -> [(String, MetaType Range)]
		              -> Either String (M.Map String NormedMetaType, NormedMetaType)
		processParams vars' [] = do
			resultType <-
				errorContext ("in return value of function at " ++
					formatRange tag)
				computeMetaType vars' result
			return (vars', resultType)
		processParams vars' ((paramName, paramType):params') = do
			paramType' <-
				errorContext ("in type of parameter \"" ++ paramName ++ "\" \
					\to function at " ++ formatRange tag) $
				normMetaType vars' paramType
			vars'' <- checkedInsert paramName paramType' vars'
			resultType <- processParams vars'' params'
			return (NMTFun paramType' resultType)
	processParams vars params
computeMetaType vars (MOVar tag name) = case M.lookup name vars of
	Just type_ -> return type_
	Nothing -> Left ("variable \"" ++ name ++ \" is not in scope")
computeMetaType vars (MOJSExpr tag type_ spec impl) = do
	shouldBeJSType <-
		errorContext ("in \"type\" clause of \"js-expr\" at " ++
			formatRange tag) $
		computeMetaType vars type_
	unless (shouldBeJSType == NMTJSType) $
		Left ("in \"js-expr\" at " ++ formatRange tag ++ ", expected the type \
			\to have meta-type " ++ formatNormedMetaType NMTJSType ++ ", but \
			\it had meta-type " ++ formatNormedMetaType shouldBeJSType)
	sequence [do
		shouldBeJSTerm <- computeMetaType vars value
		unless (shouldBeJSTerm == NMTJSTerm) $
			Left ("in \"impl\" clause of \"js-expr\" at " ++ formatRange tag ++
				", in \"(set " ++ show name ++ " ...)\" clause, value should \
				\have meta-type " ++ formatNormedMetaType NMTJSTerm ++ ", but \
				\it had meta-type " ++ formatNormedMetaType shouldBeJSTerm)
		| (name, JBVSet value) <- varsOfJavascriptBlock impl]
	-- TODO: Check validity of spec clause.
	return (NMTJSType (mapTagsOfMetaObject (const ()) type_))

-- A `JSType` is a Javascript representation of an SL type.

data JSType

	-- `JSType`s can only come from `js-repr` directives. `nameOfJSType` is the
	-- name of the original directive that produced this JS type.
	-- `paramsOfJSType` are what the parameters came out to.
	= JSType {
		nameOfJSType :: String,
		paramsOfJSType :: [JSType]
	}

	-- This is used for dependent type checking.
	| JSTypePlaceholder String

-- A `ReducedMetaObject` is the result of resolving variable references and
-- function applications in a `MetaObject` until no further reductions can be
-- performed.

data ReducedMetaObject

	-- If the original `MetaObject`'s meta-type was `fun ...`, then the result
	-- of reduction will be `RMOFun`. Unlike `MOFun`, `RMOFun` is curried. The
	-- reason its return value is wrapped in `GenSym` is so that we can
	-- generate unique variable substitutions for any Javascript chunks
	-- contained within for every time we invoke the function.
	= RMOFun (ReducedMetaObject -> GenSym ReducedMetaObject)

	-- If the original `MetaObject`'s meta-type was `type`, then the result of
	-- reduction will be `RMOJSType`.
	| RMOJSType JSType

	-- If the original `MetaObject`'s meta-type was `term ...`, then it must
	-- eventually resolve to a `js-expr` or substitution term.
	-- `jsEquivalentOfJSTerm` is the Javascript equivalent of that term.
	| RMOJSTerm {
		jsEquivalentOfJSTerm :: JS.Expression ()
	}

-- `reduce` reduces a `MetaObject` to a `ReducedMetaObject`. Because recursion
-- is not allowed in TL, it should always terminate. There are three pieces of
-- context involved:
--   * TL variables in scope (first argument)
--   * Javascript substitutions in scope (second argument), generated by `free`
--     or `set` clauses of a `js-expr` block that we are inside of
--   * `GenSym` monad so that we can generate unique variable names when we
--     encounter a `free` clause

reduce :: M.Map String ReducedMetaObject -> M.Map String (JS.Expression ()) -> MetaObject a -> GenSym ReducedMetaObject
reduce vars jsVars (MOApp { funOfMetaObject = f, argOfMetaObject = a }) = do
	RMOFun f' <- reduce vars jsVars f
	a' <- reduce vars jsVars a
	f' a'
reduce vars jsVars (MOAbs { paramsOfMetaObject = params, resultOfMetaObject = result }) = build vars params
	where
		build vars [] =
			reduce vars jsVars result
		build vars ((paramName, paramType):params) =
			return $ RMOFun (\paramValue -> build (M.insert paramName paramValue vars) params)
reduce vars jsVars (MOVar { varOfMetaObject = var }) = case M.lookup var vars of
	Just value -> return value
	Nothing -> error ("var not in scope: " ++ show var)
reduce vars jsVars (MOJSExpr { specOfMetaObject = s, implOfMetaObject = b }) = do
	jsEquivalent <- expandJavascriptBlock vars jsVars b
	return $ RMOJSTerm {
		jsEquivalentOfJSTerm = jsEquivalent
	}
reduce vars jsVars (MOJSSubstitution { jsSubstitutionOfMetaObject = x }) =
	return $ RMOJSTerm {
		jsEquivalentOfJSTerm = substituteJSVars jsVars (JS.removeAnnotations x)
	}

-- `safeUnion`, `safeFromList`, and `safeInsert` are like `M.union`,
-- `M.fromList`, and `M.insert` except that if there is a duplicate value, they
-- call `error` instead of silently overwriting it. In the future we should
-- report such errors to the user instead of crashing, but that's for some
-- other day.

safeUnion :: (Ord k, Show k) => M.Map k v -> M.Map k v -> M.Map k v
safeUnion = M.unionWithKey (\k -> error ("redefinition of " ++ show k))

safeFromList :: (Ord k, Show k) => [(k, v)] -> M.Map k v
safeFromList = foldr (uncurry safeInsert) M.empty

safeInsert :: (Ord k, Show k) => k -> v -> M.Map k v -> M.Map k v
safeInsert k v m = case M.lookup k m of
	Just _ -> error ("redefinition of " ++ show k)
	Nothing -> M.insert k v m

-- `expandJavascriptBlock` performs the variable substitutions in the given
-- `JavascriptBlock` and returns the result as a `JS.Expression`.

expandJavascriptBlock :: M.Map String ReducedMetaObject -> M.Map String (JS.Expression ()) -> JavascriptBlock a -> GenSym (JS.Expression ())
expandJavascriptBlock vars jsVars (JavascriptBlock { codeOfJavascriptBlock = code, varsOfJavascriptBlock = jsvs }) = do
	freeVars <- liftM safeFromList $ sequence [do
		uniqueSymbol <- genSym
		return (name, JS.VarRef () (JS.Id () (name ++ uniqueSymbol)))
		| (name, JBVFree) <- jsvs]
	let bannedVars = safeFromList [(name, error "ambiguous substitution") | (name, JBVSet _) <- jsvs]
	setVars <- liftM safeFromList $ sequence [do
		reduced <- reduce vars (jsVars `safeUnion` freeVars `safeUnion` bannedVars) value
		return (name, jsEquivalentOfJSTerm reduced)
		| (name, JBVSet value) <- jsvs]
	return (substituteJSVars (jsVars `safeUnion` freeVars `safeUnion` setVars) (JS.removeAnnotations code))

-- `substituteJSVars` recursively traverses the given `JS.Expression`. Every
-- time it encounters a variable reference, it looks to see if the variable is
-- in the given substitution map. If it is, then it replaces the variable with
-- the given substitution. It does not mess with attribute or label names. When
-- it encounters variable names as part of a formal parameter list to a
-- function, in an assignment, or in some other context where they are being
-- bound to, it requires that the substitution either not replace that variable
-- replace that variable with a single other variable.

substituteJSVars :: M.Map String (JS.Expression ()) -> JS.Expression () -> JS.Expression ()
substituteJSVars jsVars = sE
	where
		-- `sE` performs substitution on an expression. `sS` performs
		-- substitution on a statement. `sL` performs substitution on an
		-- l-value. `sI` and `sI'` perform substition on variables expressed as
		-- `JS.Id` and `String` respectively. `sV` performs substitutions on a
		-- `JS.VarDecl`.

		sE :: JS.Expression () -> JS.Expression ()
		sE (JS.StringLit () s) = JS.StringLit () s
		sE (JS.RegexpLit () s g i) = JS.RegexpLit () s g i
		sE (JS.NumLit () v) = JS.NumLit () v
		sE (JS.IntLit () v) = JS.IntLit () v
		sE (JS.BoolLit () v) = JS.BoolLit () v
		sE (JS.NullLit ()) = JS.NullLit ()
		sE (JS.ArrayLit () members) = JS.ArrayLit () (map sE members)
		sE (JS.ObjectLit () fields) = JS.ObjectLit () [(key, sE value) | (key, value) <- fields]
		sE (JS.ThisRef ()) = JS.ThisRef ()
		sE (JS.VarRef () (JS.Id () n)) = case M.lookup n jsVars of
			Just newValue -> newValue
			Nothing -> JS.VarRef () (JS.Id () n)
		sE (JS.DotRef () obj id) = JS.DotRef () (sE obj) id
		sE (JS.BracketRef () a b) = JS.BracketRef () (sE a) (sE b)
		sE (JS.NewExpr () thing params) = JS.NewExpr () (sE thing) (map sE params)
		sE (JS.PrefixExpr () op thing) = JS.PrefixExpr () op (sE thing)
		sE (JS.UnaryAssignExpr () op a) = JS.UnaryAssignExpr () op (sL a)
		sE (JS.InfixExpr () op a b) = JS.InfixExpr () op (sE a) (sE b)
		sE (JS.CondExpr () test true false) = JS.CondExpr () (sE test) (sE true) (sE false)
		sE (JS.AssignExpr () op a b) = JS.AssignExpr () op (sL a) (sE b)
		sE (JS.ListExpr () things) = JS.ListExpr () (map sE things)
		sE (JS.CallExpr () fun args) = JS.CallExpr () (sE fun) (map sE args)
		sE (JS.FuncExpr () name args stmts) = JS.FuncExpr () (fmap sI name) (map sI args) (map sS stmts)

		sL :: JS.LValue () -> JS.LValue ()
		sL (JS.LVar () i) = JS.LVar () (sI' i)
		sL (JS.LDot () l r) = JS.LDot () (sE l) r
		sL (JS.LBracket () l r) = JS.LBracket () (sE l) (sE r)

		sI :: JS.Id () -> JS.Id ()
		sI (JS.Id () s) = JS.Id () (sI' s)

		-- TODO: This can be done better. Instead of assuming that it came from
		-- a `free`-clause if it is just one variable, we should separately
		-- store and handle variables that came from `free`-clauses.
		-- Or maybe we should automatically relabel all Javascript variables
		-- that we see entering scope, and then do away with `free`-clauses
		-- altogether.
		sI' :: String -> String
		sI' s = case M.lookup s jsVars of
			Nothing -> s
			Just (JS.VarRef () (JS.Id () t)) -> t
			Just _ -> error "substitute variable for non-variable"

		sS :: JS.Statement () -> JS.Statement ()
		sS (JS.BlockStmt () stmts) = JS.BlockStmt () (map sS stmts)
		sS (JS.EmptyStmt ()) = JS.EmptyStmt ()
		sS (JS.ExprStmt () e) = JS.ExprStmt () (sE e)
		sS (JS.IfStmt () test true false) = JS.IfStmt () (sE test) (sS true) (sS false)
		sS (JS.IfSingleStmt () test true) = JS.IfSingleStmt () (sE test) (sS true)
		sS (JS.SwitchStmt () subject clauses) = JS.SwitchStmt () (sE subject) (map sC clauses)
			where
				sC :: JS.CaseClause () -> JS.CaseClause ()
				sC (JS.CaseClause () key stmts) = JS.CaseClause () (sE key) (map sS stmts)
				sC (JS.CaseDefault () stmts) = JS.CaseDefault () (map sS stmts)
		sS (JS.WhileStmt () test body) = JS.WhileStmt () (sE test) (sS body)
		sS (JS.DoWhileStmt () body test) = JS.DoWhileStmt () (sS body) (sE test)
		sS (JS.BreakStmt () label) = JS.BreakStmt () label
		sS (JS.ContinueStmt () label) = JS.ContinueStmt () label
		sS (JS.LabelledStmt () label stmt) = JS.LabelledStmt () label (sS stmt)
		sS (JS.ForInStmt () init subject stmt) = JS.ForInStmt () (sFII init) (sE subject) (sS stmt)
			where
				sFII :: JS.ForInInit () -> JS.ForInInit ()
				sFII (JS.ForInVar v) = JS.ForInVar (sI v)
				sFII (JS.ForInLVal lv) = JS.ForInLVal (sL lv)
		sS (JS.ForStmt () init test step stmt) = JS.ForStmt () (sFI init) (fmap sE test) (fmap sE step) (sS stmt)
			where
				sFI :: JS.ForInit () -> JS.ForInit ()
				sFI JS.NoInit = JS.NoInit
				sFI (JS.VarInit vars) = JS.VarInit (map sV vars)
				sFI (JS.ExprInit e) = JS.ExprInit (sE e)
		sS (JS.TryStmt () content catch finally) = JS.TryStmt () (sS content) (fmap sCC catch) (fmap sS finally)
			where
				sCC :: JS.CatchClause () -> JS.CatchClause ()
				sCC (JS.CatchClause () i s) = JS.CatchClause () (sI i) (sS s)
		sS (JS.ThrowStmt () e) = JS.ThrowStmt () (sE e)
		sS (JS.ReturnStmt () e) = JS.ReturnStmt () (fmap sE e)
		sS (JS.WithStmt () subject stmt) = JS.WithStmt () (sE subject) (sS stmt)
		sS (JS.VarDeclStmt () vars) = JS.VarDeclStmt () (map sV vars)
		sS (JS.FunctionStmt () name vars body) = JS.FunctionStmt () (sI name) (map sI vars) (map sS body)

		sV :: JS.VarDecl () -> JS.VarDecl ()
		sV (JS.VarDecl () name value) = JS.VarDecl () (sI name) (fmap sE value)

-- `processDirective` processes a single TL directive. As input, it takes the
-- map of globally defined meta-objects before the directive. As output, it
-- returns the map of globally defined meta-objects after the directive.

processDirective :: M.Map String ReducedMetaObject -> Directive a -> GenSym (M.Map String ReducedMetaObject)
processDirective vars directive@(DLet { }) = do
	let
		applyParams vars' [] =
			reduce vars' M.empty (valueOfDirective directive)
		applyParams vars' ((n, t):rest) = return $ RMOFun $ \ value ->
			applyParams (safeInsert n value vars') rest 
	newValue <- applyParams vars (paramsOfDirective directive)
	return $ safeInsert (nameOfDirective directive) newValue vars
processDirective vars directive@(DJSRepr { }) =
	undefined

