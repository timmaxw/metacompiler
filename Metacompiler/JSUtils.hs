module Metacompiler.JSUtils where

import Control.Applicative
import Control.Monad.State.Lazy hiding (sequence, mapM)
import qualified Data.Map as M
import Data.Traversable
import Language.ECMAScript3.Syntax
import Prelude hiding (sequence, mapM)

-- `traverseExpression`, `traverseLValue`, and `traverseStatement` are very
-- general functions that can be used to implement all sorts of traversals of
-- the JavaScript AST. Each one takes a `Visitor`; for each sub-node of the
-- AST, it calls the appropriate `visit*` function. The return value from the
-- `visit*` function is substituted into the AST at that point. Typically the
-- `visit*` function will check something and recursively call `traverse*`
-- again. In addition, the entire thing is parameterized on an applicative
-- functor; this makes it possible to e.g. assemble a set of all the variable
-- names mentioned by using the `Writer` monad as an applicative functor.

data Visitor f a = Visitor {
	visitExpression :: Expression a -> f (Expression a),
	visitLValue :: LValue a -> f (LValue a),
	visitStatement :: Statement a -> f (Statement a)
	}

-- `defaultVisitor` is a convenience function for constructing a `Visitor`.
-- Users are advised to define `visitor = (defaultVisitor visitor) {...}` so
-- that they only need to override the `visit*` functions that they care about.

defaultVisitor :: Applicative f => Visitor f a -> Visitor f a
defaultVisitor subVisitor = Visitor {
	visitExpression = traverseExpression subVisitor,
	visitLValue = traverseLValue subVisitor,
	visitStatement = traverseStatement subVisitor
	}

-- `traverseExpression` calls the appropriate `visit*` function on every
-- sub-node of the given expression, substitutes the return values in, and then
-- returns the result.

traverseExpression :: Applicative f => Visitor f a -> Expression a -> f (Expression a)
traverseExpression visitor expression = case expression of
	StringLit a s -> pure (StringLit a s)
	RegexpLit a s g i -> pure (RegexpLit a s g i)
	NumLit a v -> pure (NumLit a v)
	IntLit a v -> pure (IntLit a v)
	BoolLit a v -> pure (BoolLit a v)
	NullLit a -> pure (NullLit a)
	ArrayLit a m -> liftA (ArrayLit a) (traverse visitE m)
	ObjectLit a m -> liftA (ObjectLit a) (sequenceA [liftA ((,) key) (visitE v) | (key, v) <- m])
	ThisRef a -> pure (ThisRef a)
	VarRef a i -> pure (VarRef a i)
	DotRef a obj id -> liftA2 (DotRef a) (visitE obj) (pure id)
	BracketRef a obj ix -> liftA2 (BracketRef a) (visitE obj) (visitE ix)
	NewExpr a fun params -> liftA2 (NewExpr a) (visitE fun) (traverse visitE params)
	PrefixExpr a op x -> liftA (PrefixExpr a op) (visitE x)
	UnaryAssignExpr a op x -> liftA (UnaryAssignExpr a op) (visitLValue visitor x)
	InfixExpr a op x y -> liftA2 (InfixExpr a op) (visitE x) (visitE y)
	CondExpr a test true false -> liftA3 (CondExpr a) (visitE test) (visitE true) (visitE false)
	AssignExpr a op x y -> liftA2 (AssignExpr a op) (visitLValue visitor x) (visitE y)
	ListExpr a m -> liftA (ListExpr a) (traverse visitE m)
	CallExpr a fun args -> liftA2 (CallExpr a) (visitE fun) (traverse visitE args)
	FuncExpr a name args body -> liftA (FuncExpr a name args) (traverse (visitStatement visitor) body)
	where
		visitE = visitExpression visitor

-- `traverseLValue` calls the appropriate `visit*` function on every sub-node
-- of the given L-value, substitutes the return values in, and then returns the
-- result.

traverseLValue :: Applicative f => Visitor f a -> LValue a -> f (LValue a)
traverseLValue visitor lvalue = case lvalue of
	LVar a i -> pure (LVar a i)
	LDot a obj id -> liftA2 (LDot a) (visitExpression visitor obj) (pure id)
	LBracket a obj ix -> liftA2 (LBracket a) (visitExpression visitor obj) (visitExpression visitor ix)

-- `traverseStatement` calls the appropriate `visit*` function on every
-- sub-node of the given L-value, substitutes the return values in, and then
-- returns the result.

traverseStatement :: Applicative f => Visitor f a -> Statement a -> f (Statement a)
traverseStatement visitor statement = case statement of
	BlockStmt a stmts -> liftA (BlockStmt a) (traverse visitS stmts)
	EmptyStmt a -> pure (EmptyStmt a)
	ExprStmt a e -> liftA (ExprStmt a) (visitE e)
	IfStmt a test true false -> liftA3 (IfStmt a) (visitE test) (visitS true) (visitS false)
	IfSingleStmt a test true -> liftA2 (IfSingleStmt a) (visitE test) (visitS true)
	SwitchStmt a subj clauses -> liftA2 (SwitchStmt a) (visitE subj) (traverse visitClause clauses)
		where
			visitClause (CaseClause a key stmts) = liftA (CaseClause a key) (traverse visitS stmts)
			visitClause (CaseDefault a stmts) = liftA (CaseDefault a) (traverse visitS stmts)
	WhileStmt a test body -> liftA2 (WhileStmt a) (visitE test) (visitS body)
	DoWhileStmt a body test -> liftA2 (DoWhileStmt a) (visitS body) (visitE test)
	BreakStmt a label -> pure (BreakStmt a label)
	ContinueStmt a label -> pure (ContinueStmt a label)
	LabelledStmt a label stmt -> liftA (LabelledStmt a label) (visitS stmt)
	ForInStmt a init subj body -> liftA3 (ForInStmt a) init' (visitE subj) (visitS body)
		where
			init' = case init of
				ForInVar v -> pure (ForInVar v)
				ForInLVal l -> liftA ForInLVal (visitLValue visitor l)
	ForStmt a init test step body ->
		pure (ForStmt a) <*> init' <*> traverse visitE test <*> traverse visitE step <*> visitS body
		where
			init' = case init of
				NoInit -> pure NoInit
				VarInit vars -> pure (VarInit vars)
				ExprInit e -> liftA ExprInit (visitE e)
	TryStmt a body Nothing finally ->
		liftA3 (TryStmt a) (visitS body) (pure Nothing) (traverse visitS finally)
	TryStmt a body (Just (CatchClause a2 exc catch)) finally ->
		liftA3 (TryStmt a) (visitS body) (liftA (Just . CatchClause a2 exc) (visitS catch)) (traverse visitS finally)
	ThrowStmt a exc -> liftA (ThrowStmt a) (visitE exc)
	ReturnStmt a val -> liftA (ReturnStmt a) (traverse visitE val)
	WithStmt a subj body -> liftA2 (WithStmt a) (visitE subj) (visitS body)
	VarDeclStmt a vars -> liftA (VarDeclStmt a)
		(sequenceA [liftA (VarDecl a name) (traverse visitE value) | VarDecl a name value <- vars])
	FunctionStmt a name args body -> liftA (FunctionStmt a name args) (traverse visitS body)
	where
		visitE = visitExpression visitor
		visitS = visitStatement visitor

-- "Revariabling" an AST means to recursively traverse the AST, replacing each
-- bound variable with a newly-generated symbol and also making any
-- substitutions specified by the given mapping. The reason why these two
-- behaviors are rolled together is that replacing bound variables with newly-
-- generated symbols requires the ability to substitute recursively.

-- The `State SymbolRenaming` monad is used to generate unique names for
-- symbols.

data SymbolRenaming = SymbolRenaming Int

renameSymbol :: String -> State SymbolRenaming String
renameSymbol original = do
	SymbolRenaming num <- get
	put (SymbolRenaming (num + 1))
	return (original ++ "_" ++ show num)

initialSymbolRenaming :: SymbolRenaming
initialSymbolRenaming = SymbolRenaming 0

-- `makeRevariableVisitor` creates a `Visitor` that will revariable whatever
-- expression or statement it is applied to, with the given substitutions.

makeRevariableVisitor :: M.Map String (Expression ()) -> Visitor (State SymbolRenaming) ()
makeRevariableVisitor subs = (defaultVisitor (makeRevariableVisitor subs)) {
	visitExpression = revariableExpression subs,
	visitStatement = revariableStatement subs
	}

-- `revariableScope` revariables the root statement of a new scope. The first
-- parameter is a series of substitutions to make. It begins by recursively
-- looking for `var` statements in the new scope; it replaces each variable it
-- finds this way with a new unique variable. Then it applies the subsitutions
-- generated by this process, along with any substitutions from the first
-- parameter, recursively.
--
-- The second parameter can be used to "seed" the map of `var` statements in
-- the new scope. This is so that in a construct like
--     function (x) { var x, y; }
-- the system will realize that the two `x`s are the same. Specifically, in
-- that case, `revariableScope` would be called on `{ var x; }` with the
-- mapping "`x` -> `x_1`" as its second parameter. The return value of
-- `revariableScope` would be something like:
--     { var x_1, y_2; }
-- Whatever called it (`revariableExpression` or `revariableStatement`) would
-- have performed the corresponding substitution on the argument list, so the
-- end result would be:
--     function (x_1) { var x_1, y_1; }

revariableScope :: M.Map String (Expression ()) -> M.Map String String -> [Statement ()] -> State SymbolRenaming [Statement ()]
revariableScope subs paramSubs root = do
	(root', varSubs) <- runStateT (mapM (traverseStatement lookForVarDecls) root) paramSubs
	let subs' = M.union subs (M.map (VarRef () . Id ()) varSubs)
	traverse (revariableStatement subs') root'

-- `revariableExpression` revariables the given expression.

revariableExpression :: M.Map String (Expression ()) -> Expression () -> State SymbolRenaming (Expression ())
revariableExpression subs (VarRef () (Id () name)) = case M.lookup name subs of
	Just value -> return value
	Nothing -> return (VarRef () (Id () name))
revariableExpression subs (FuncExpr () name params body) = do
	let paramNames = [str | Id () str <- params]
	paramNames' <- mapM renameSymbol paramNames
	let paramSubs = M.fromList (zip paramNames paramNames')
	body' <- revariableScope subs paramSubs body
	return (FuncExpr () name [Id () str | str <- paramNames'] body')
revariableExpression subs other = traverseExpression (makeRevariableVisitor subs) other

-- `revariableStatement` revariables the given statement.

revariableStatement :: M.Map String (Expression ()) -> Statement () -> State SymbolRenaming (Statement ())
revariableStatement subs (FunctionStmt () name params body) = do
	let paramNames = [str | Id () str <- params]
	paramNames' <- mapM renameSymbol paramNames
	let paramSubs = M.fromList (zip paramNames paramNames')
	body' <- revariableScope subs paramSubs body
	return (FunctionStmt () name [Id () str | str <- paramNames'] body')
revariableStatement subs (TryStmt () body (Just (CatchClause () (Id () exc) catch)) finally) = do
	body' <- revariableStatement subs body
	exc' <- renameSymbol exc
	let subs' = M.insert exc (VarRef () (Id () exc')) subs
	catch' <- revariableStatement subs' catch
	finally' <- traverse (revariableStatement subs) finally
	return (TryStmt () body' (Just (CatchClause () (Id () exc') catch')) finally')
revariableStatement subs other = traverseStatement (makeRevariableVisitor subs) other

-- `lookForVarDecls` recursively searches the given statement for `var`
-- statements, `for (var in ...)` constructs, and other such equivalent things.
-- As it searches, it keeps track of a map from old variables to new variables.
-- When it finds a `var`-construct, it will generate a new variable, replace
-- the `var`-construct's variable with it, and update the map. But if the old
-- variable was already in the map, then it will use the entry for the old
-- variable in place of generating a new one. For example, `var x, x;` would be
-- transformed to `var x_1, x_1;` or something like that. 

lookForVarDecls :: Visitor (StateT (M.Map String String) (State SymbolRenaming)) ()
lookForVarDecls = (defaultVisitor lookForVarDecls) {
	visitStatement = (\ statement -> case statement of
		VarDeclStmt () vars -> do
			vars' <- sequence [do
				var' <- foundAVarDecl var
				value' <- traverse (traverseExpression lookForVarDecls) value
				return (VarDecl () (Id () var') value')
				| VarDecl () (Id () var) value <- vars]
			return (VarDeclStmt () vars)
		ForInStmt () (ForInVar (Id () var)) subj body -> do
			var' <- foundAVarDecl var
			subj' <- traverseExpression lookForVarDecls subj
			body' <- traverseStatement lookForVarDecls body
			return (ForInStmt () (ForInVar (Id () var')) subj' body')
		ForStmt () (VarInit vars) test step body -> do
			vars' <- sequence [do
				var' <- foundAVarDecl var
				return (VarDecl () (Id () var') value)
				| VarDecl () (Id () var) value <- vars]
			test' <- traverse (traverseExpression lookForVarDecls) test
			step' <- traverse (traverseExpression lookForVarDecls) step
			body' <- traverseStatement lookForVarDecls body
			return (ForStmt () (VarInit vars') test' step' body')
		FunctionStmt () name params body ->
			return (FunctionStmt () name params body)
		_ -> traverseStatement lookForVarDecls statement
		),
	visitExpression = (\ expression -> case expression of
		FuncExpr () name params body ->
			return (FuncExpr () name params body)
		_ -> traverseExpression lookForVarDecls expression
		)
	}
	where
		foundAVarDecl :: String -> StateT (M.Map String String) (State SymbolRenaming) String
		foundAVarDecl old = do
			oldState <- get
			case M.lookup old oldState of
				Nothing -> do
					new <- lift (renameSymbol old)
					put (M.insert old new oldState)
					return new
				Just alreadySubstituted -> return alreadySubstituted


