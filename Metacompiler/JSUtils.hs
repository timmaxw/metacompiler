module Metacompiler.JSUtils (
	Visitor(..), defaultVisitor, traverseExpression, traverseLValue, traverseStatement,
	freeNamesInExpression, freeNamesInStatement, freeNamesInScope,
	Subst(..), substituteExpression, substituteLValue, substituteStatement, substituteScope
	) where

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

-- `freeNamesInExpression` and `freeNamesInStatement` returns all of the unbound identifiers that are referenced in
-- the given expression or statement.

freeNamesInExpression :: Ord a => Expression a -> S.Set (Id a)
freeNamesInExpression (VarRef _ i) =
	S.singleton i
freeNamesInExpression (FuncExpr _ _ args body) =
	freeNamesInScope body \\ S.fromList args
freeNamesInExpression other =
	execWriter (traverseExpression freeNamesVisitor other)

freeNamesInStatement :: Ord a => Statement a -> S.Set (Id a)
freeNamesInStatement (FunctionStmt _ _ args body) =
	freeNamesInScope body S.fromList args
freeNamesInStatement (TryStmt _ body (Just (CatchClause _ i catch)) finally) =
	freeNamesInStatement body
	`S.union` S.delete i (freeNamesInStatement catch)
	`S.union` maybe S.empty freeNamesInStatement finally
freeNamesInStatement other =
	execWriter (traverseStatement freeNamesVisitor other)

freeNamesInScope :: Ord a => [Statement a] -> S.Set (Id a)
freeNamesInScope scope =
	S.unions (map freeNamesInStatement scope) \\
		S.unions (map varDeclsInStatement scope)

freeNamesVisitor :: Ord a => Visitor (Writer (S.Set (Id a))) a
freeNamesVisitor = (defaultVisitor freeNamesVisitor) {
	visitExpression = freeNamesInExpression,
	visitStatement = freeNamesInStatement
	}

varDeclsInStatement :: Statement a -> S.Set (Id a)
varDeclsInStatement (VarDeclStmt _ vars) =
	S.fromList [i | VarDecl _ i _ <- vars]
varDeclsInStatement (ForInStmt _ (ForInVar i) _ body) =
	S.insert i (varDeclsInStatement body)
varDeclsInStatement (ForStmt _ (VarInit vars) _ _ body) =
	S.fromList [i | VarDecl _ i _ <- vars] `S.union` varDeclsInStatement body
varDeclsInStatement (FunctionStmt _ _ _ _) =
	S.empty
varDeclsInStatement other =
	execWriter (traverseStatement varDeclsVisitor other)

varDeclsVisitor :: Ord a => Visitor (Writer (S.Set (Id a))) a
varDeclsVisitor = (defaultVisitor varDeclsVisitor) {
	visitExpression = const S.empty,
	visitStatement = varDeclsInStatement
	}

-- `substitute*` traverses the given Javascript expression or statement, looking for mentions of the variables in the
-- given map. What happens when it finds a variable depends on the value in the map:
--   * `SubstValue` means that the variable will be replaced with the given expression. If it appears in an assignment
--     context, that's an error.
--   * `SubstFun` means that the variable should appear invoked like a function. The function of `SubstFun` will be
--     called with the arguments to the function invocation, and the result substituted in place of the variable. If
--     the variable appears in any other context, that's an error. The other parameter to the `SubstFun` constructor is
--     a set of variables; the return value of the function shouldn't contain free variables not in this set unless
--     those variables appeared in the arguments to the function.
--   * `SubstId` means that the variable will be replaced with the given other variable even if it appears on the
--     left-hand side of an assignment.
-- Note that `substitute*` will recognize when a new variable is being introduced into scope, and will treat it as a
-- different variable. For example, replacing `"x"` with `SubstId "y"` in "[x = f(x), function(x) { return x; }]" will
-- return "[y = f(y), function(x) { return x; }]" because the "x" in the function is a different "x".
--
-- `substituteStatement` and `substituteScope` take an additional parameter `declSubs`. If any of the keys in
-- `declSubs` appear on the left side of a `var` declaration, then they will be replaced with the corresponding value
-- in `declSubs`.

data Subst
	= SubstValue (Expression ())
	| SubstFun ([Expression ()] -> Expression ()) (S.Set (Id ()))
	| SubstId (Id ())

substituteExpression :: M.Map (Id ()) Subst -> Expression () -> Expression ()
substituteExpression subs (VarRef _ i) = case M.lookup i subs of
	Just (SubstValue v) -> v
	Just (SubstFun _ _) -> error "found a SubstFun not being invoked like a function"
	Just (SubstId i') -> VarRef () i'
	Nothing -> VarRef () i
substituteExpression subs (CallExpr _ (VarRef _ i) args) = case M.lookup i subs of
	Just (SubstValue v) -> CallExpr () v args'
	Just (SubstFun fun allowedFreeVars) -> let v = fun args' in
		if freeNamesInExpression v `S.isSubsetOf` (allowedFreeVars `S.union` S.unions (map freeNamesInExpression args'))
			then v
			else error "free vars promise is incorrect"
	Just (SubstId i') -> CallExpr () (VarRef () i') args'
	Nothing -> CallExpr () (VarRef () i) args'
	where
		args' = map (substituteExpression subs) args
substituteExpression subs (FuncExpr _ name args body) = let
	name' = case M.lookup name subs of
		Just (SubstId i') -> i'
		Just _ -> error "variable to be substituted appears in assignment context"
		Nothing -> name
	freeInBody = freeNamesInScope body
	(subs', args') = prepareForBindingNames freeInBody (subs, args)
	body' = substituteScope subs' (M.fromList (zip args args')) body
	in FuncExpr () name' args' body'
substituteExpression subs other =
	runIdentity (traverseExpression (substitutionVisitor subs) other)

substituteLValue :: M.Map (Id ()) Subst -> LValue () -> LValue ()
substituteLValue subs (LVar _ i) = case M.lookup i subs of
	Just (SubstValue _) -> error "variable to be substituted appears in assignment context"
	Just (SubstFun _ _) -> error "variable to be substituted appears in assignment context"
	Just (SubstId i') -> LVar () i'
	Nothing -> LVar () i
substituteLValue subs other =
	runIdentity (traverseLValue (substitutionVisitor subs) other)

substitutionVisitor :: M.Map (Id ()) Subst -> Visitor Identity ()
substitutionVisitor subs = Visitor {
	visitExpression = substituteExpression subs,
	visitLValue = substituteLValue subs,
	visitStatement = error "we shouldn't get from an expression to a statement without going through FuncExpr"
	}

substituteStatement :: M.Map (Id ()) Subst -> M.Map (Id ()) (Id ()) -> Statement () -> Statement ()
substituteStatement subs declSubs (VarDeclStmt _ vars) = let
	vars' = [let
		i' = case M.lookup i declSubs of
			Just i' -> i'
			Nothing -> i
		v' = substituteExpression subs v
		in VarDecl i' v'
		| VarDecl _ i v <- vars]
	in VarDeclStmt () vars'
substituteStatement subs declSubs (ForInStmt _ (ForInVar i) subj body) = let
	i' = case M.lookup i declSubs of
		Just i' -> i'
		Nothing -> i
	subj' = substituteExpression subs subj
	body' = substituteStatement subs declSubs body
	in ForInStmt () (ForInVar i') subj' body'
substituteStatement subs declSubs (ForStmt _ (VarInit vars) test step body) = let
	vars' = [let
		i' = case M.lookup i declSubs of
			Just i' -> i'
			Nothing -> i
		v' = substituteExpression subs v
		in VarDecl i' v'
		| VarDecl _ i v <- vars]
	test' = substituteExpression subs test
	step' = substituteExpression subs step
	body' = substituteStatement subs declSubs body
	in ForStmt () (VarInit vars') test' step' body'
substituteStatement subs declSubs (FunctionStmt _ name args body) = let
	name' = case M.lookup name subs of
		Just (SubstId i') -> i'
		Just _ -> error "variable to be substituted appears in assignment context"
		Nothing -> name
	freeInBody = freeNamesInScope body
	(subs', args') = prepareForBindingNames freeInBody (subs, args)
	body' = substituteScope subs' (M.fromList (zip args args')) body
	in FunctionStmt () name' args' body'
substituteStatement subs declSubs other =
	runIdentity (traverseStatement (substitutionWithDeclSubsVisitor subs declSubs) other)

substitutionWithDeclSubsVisitor :: M.Map (Id ()) Subst -> M.Map (Id ()) (Id ()) -> Visitor Identity ()
substitutionWithDeclSubsVisitor subs declSubs = Visitor {
	visitExpression = substituteExpression subs,
	visitLValue = substituteLValue subs,
	visitStatement = substituteStatement subs declSubs
	}

substituteScope :: M.Map (Id ()) Subst -> M.Map (Id ()) (Id ()) -> [Statement ()] -> [Statement ()]
substituteScope subs declSubs scope = let
	decls = S.unions (map varDeclsInStatement scope)
	newDecls = S.toList (S.filter (`M.notMember` declSubs) decls)
	freeInBody = S.unions (map freeNamesInStatement scope)
	(subs', newDecls') = prepareForBindingNames freeInBody (subs, newDecls)
	declSubs' = M.fromList (zip newDecls newDecls') `M.union` declSubs
	in map (substituteStatement subs' declSubs') scope

prepareForBindingNames :: S.Set (Id ())
                       -> (M.Map (Id ()) Subst, [Name])
                       -> (M.Map (Id ()) Subst, [Name])
prepareForBindingNames freeNamesWithin (subs, names) = let
	forbidden = S.unions [
		case M.lookup name subs of
			Just (SubstValue x) -> freeNamesInExpression x
			Just (SubstFun _ x) -> x
			Just (SubstId i) -> S.singleton i
			Nothing -> S.singleton name
		| name <- S.toList (freeNamesWithin \\ S.fromList names)]
	names' = snd (mapAccumL
		(\ forbidden name -> let
			candidates = [Id () (unId name ++ replicate n '\'') | n <- [0..]]
			name' = find (`S.notMember` forbidden) candidates
			in (S.insert name' forbidden, name'))
		forbidden
		names)
	subs' = M.fromList (zip names (map SubstId names'))
		`M.union` M.filter (`notElem` names') subs
	in (subs', names')

