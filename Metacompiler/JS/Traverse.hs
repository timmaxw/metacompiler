module Metacompiler.JS.Traverse (
	Visitor(..), defaultVisitor, traverseExpression, traverseLValue, traverseStatement
	) where

import Control.Applicative
import Data.Traversable
import Language.ECMAScript3.Syntax

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

