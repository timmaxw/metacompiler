module Metacompiler.JSUtils where

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

	-- `visitStatements` is used in place of `visitStatement` any time that
	-- there is a block of multiple statements, so that each statement can
	-- affect how future statements in the same block are processed.
	visitStatements :: [Statement a] -> f [Statement a]
	}

-- `defaultVisitor` is a convenience function for constructing a `Visitor`.
-- Users are advised to define `visitor = (defaultVisitor visitor) {...}` so
-- that they only need to override the `visit*` functions that they care about.

defaultVisitor :: Visitor f a -> Visitor f a
defaultVisitor subVisitor = Visitor {
	visitExpression = traverseExpression subVisitor,
	visitLValue = traverseLValue subVisitor,
	visitStatement = traverseStatement subVisitor,
	visitStatements = traverse (traverseStatement subVisitor)
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
	AssignExpr a op x y = liftA2 (AssignExpr a op) (visitLValue visitor x) (visitE y)
	ListExpr a m -> liftA (ListExpr a) (traverse visitE m)
	CallExpr a fun args -> liftA2 (CallExpr a) (visitE fun) (traverse visitE args)
	FuncExpr a name args body -> liftA (FuncExpr a name args) (visitStatements visitor body)
	where
		visitE = visitExpression visitor

-- `traverseLValue` calls the appropriate `visit*` function on every sub-node
-- of the given L-value, substitutes the return values in, and then returns the
-- result.

traverseLValue :: Applicative f => Visitor f a -> Expression a -> f (Expression a)
traverseLValue visitor lvalue = case lvalue of
	LVar a i -> pure (LVar a i)
	LDot a obj id -> liftA2 (LDot a) (visitExpression visitor obj) (pure id)
	LBracket a obj ix -> liftA2 (LBracket a) (visitExpression visitor obj) (visitExpression visitor ix)

-- `traverseStatement` calls the appropriate `visit*` function on every
-- sub-node of the given L-value, substitutes the return values in, and then
-- returns the result.

traverseStatement :: Applicative f => Visitor f a -> Statement a -> f (Statement a)
traverseStatement visitor statement = case statement of
	BlockStmt a stmts -> liftA (BlockStmt a) (visitStatements visitor stmts)
	EmptyStmt a -> pure (EmptyStmt a)
	ExprStmt a e -> liftA (ExprStmt a) (visitE e)
	IfStmt a test true false -> liftA3 (IfStmt a) (visitE test) (visitS true) (visitS false)
	IfSingleStmt a test true -> liftA2 (IfSingleStmt a) (visitE test) (visitS true)
	SwitchStmt a subj clauses -> liftA2 (SwitchStmt a) (visitE subj) (traverse visitClause clauses)
		where
			visitClause (CaseClause a key stmts) = liftA (CaseClause a key) (visitStatements visitor stmts)
			visitClause (CaseDefault a stmts) = liftA (CaseDefault a) (visitStatements visitor stmts)
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
	TryStmt a body (CatchClause a2 exc catch) finally ->
		liftA3 (TryStmt a) (visitS body) (liftA (CatchClause a2 exc) (visitS catch)) (visitS finally)
	ThrowStmt a exc -> liftA (ThrowStmt a) (visitE exc)
	ReturnStmt a val -> liftA (ReturnStmt a) (visitE val)
	WithStmt a subj body -> liftA2 (WithStmt a) (visitE subj) (visitS body)
	VarDeclStmt a vars -> liftA (VarDeclStmt a)
		(sequenceA [liftA (VarDecl a name) (traverse visitE value) | VarDecl a name value <- vars])
	FunctionStmt a name args body -> liftA (FunctionStmt a name args) (visitS body)
	where
		visitE = visitExpression visitor
		visitS = visitStatement visitor

-- "Revariabling" an AST means to recursively traverse the AST, replacing each
-- bound variable with a newly-generated symbol and also making any
-- substitutions specified by the given mapping. The reason why these two
-- behaviors are rolled together is that replacing bound variables with newly-
-- generated symbols requires the ability to substitute recursively.

makeRevariableVisitor :: M.Map String (Expression ()) -> Visitor GenSym ()
makeRevariableVisitor subs = (defaultVisitor (makeRevariableVisitor subs)) {
	visitExpression = revariableExpression subs,
	visitStatements = revariableStatements subs,
	visitStatement = liftA only . revariableStatements . (:[])
	}
	where
		only [a] = a
		only _ = error "expected only one"

revariableExpression :: M.Map String (Expression ()) -> Expression () -> GenSym (Expression ())
revariableExpression subs (VarRef () (Id () name)) = case Map.lookup name subs of
	Just value -> return value
	Nothing -> return (VarRef () (Id () name))
revariableExpression subs (FuncExpr name params body) = do
	params' <- mapM (const genSym) params
	let subs' = M.union subs (M.fromList [(old, VarRef () (Id () new)) | (old, new) <- zip params params'])
	[body'] <- revariableStatements subs' [body]
	return (FuncExpr name params' body')
revariableExpression subs other = traverseExpression (makeRevariableVisitor subs) other

revariableStatements :: M.Map String (Expression ()) -> [Statement ()] -> GenSym [Statement ()]
revariableStatements subs [] =
	return []
revariableStatements subs (VarDeclStmt () vars:stmts) = do
	(vars', subs') <- revariableVars subs vars
	stmts' <- revariableStatements subs' stmts
	return (VarDeclStmt () vars':stmts')
revariableStatements subs (stmt:stmts) = do
	stmt' <- revariableStatement subs stmt
	stmts' <- revariableStatements subs stmts
	return (stmt':stmts')

revariableStatement :: M.Map String (Expression ()) -> Statement () -> GenSym (Statement ())
revariableStatement subs (ForInStmt () (ForInVar (Id () var)) subj body) = do
	var' <- genSym
	let subs' = M.insert var (VarRef () (Id () var')) subs
	body' <- revariableStatement subs' body
	return (ForInStmt () (ForInVar (Id () var')) subj body')
revariableStatement subs (ForStmt () (VarInit vars) test step body) = do
	(vars', subs') <- revariableVars subs vars
	test' <- traverse (revariableExpression subs') test
	step' <- traverse (revariableExpression subs') step
	body' <- revariableStatement subs' body
	return (ForStmt () (VarInit vars') test' step' body')
revariableStatement subs (TryStmt () body (CatchClause () (Id () exc) catch) finally) = do
	body' <- revariableStatement subs body
	exc' <- genSym
	let subs' = M.insert exc (VarRef () (Id () exc')) subs
	catch' <- revariableStatement subs' catch
	finally' <- revariableStatement subs finally
	return (TryStmt () body' (CatchClause () (Id () exc') catch') finally')
revariableStatement subs (FuncStmt name params body) = do
	params' <- mapM (const genSym) params
	let subs' = M.union subs (M.fromList [(old, VarRef () (Id () new)) | (old, new) <- zip params params'])
	[body'] <- revariableStatements subs' [body]
	return (FuncExpr name params' body')

revariableVars :: M.Map String (Expression ()) -> [VarDecl ()] -> GenSym ([VarDecl ()], M.Map String (Expression ()))
revariableVars subs [] = return ([], subs)
revariableVars subs (VarDecl () (Id () var) value:vars) = do
	var' <- genSym
	value' <- traverse (revariableExpression subs) value
	let subs' = M.insert var (VarRef () (Id () var')) subs
	(vars', subs'') <- revariableVars subs' vars
	return (VarDecl () (Id () var') value':vars', subs'')

