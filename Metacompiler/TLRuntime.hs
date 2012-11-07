module Metacompiler.TLRuntime where

import Control.Monad (liftM)
import qualified Data.Map as M
import qualified Language.ECMAScript3.Syntax as JS
import qualified Language.ECMAScript3.Syntax.Annotations as JS
import Metacompiler.GenSym
import qualified Metacompiler.SLSyntax as SL
import Metacompiler.TLSyntax

data ReducedMetaObject
	= RMOFun
		(ReducedMetaObject -> GenSym ReducedMetaObject)
	| RMOJSType {
		nameOfJSType :: String,
		paramsOfJSType :: [ReducedMetaObject]
	}
	| RMOJSTerm {
		jsEquivalentOfJSTerm :: JS.Expression ()
	}

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

safeUnion :: (Ord k, Show k) => M.Map k v -> M.Map k v -> M.Map k v
safeUnion = M.unionWithKey (\k -> error ("redefinition of " ++ show k))

safeFromList :: (Ord k, Show k) => [(k, v)] -> M.Map k v
safeFromList = foldr (uncurry safeInsert) M.empty

safeInsert :: (Ord k, Show k) => k -> v -> M.Map k v -> M.Map k v
safeInsert k v m = case M.lookup k m of
	Just _ -> error ("redefinition of " ++ show k)
	Nothing -> M.insert k v m

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

substituteJSVars :: M.Map String (JS.Expression ()) -> JS.Expression () -> JS.Expression ()
substituteJSVars jsVars = sE
	where
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

