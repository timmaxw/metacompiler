module Metacompiler.JS.FreeNames (
	freeNamesInExpression, freeNamesInStatement, freeNamesInScope,
	varDeclsInStatement
	) where

import Control.Applicative
import qualified Data.Set as S
import Language.ECMAScript3.Syntax
import Metacompiler.JS.Traverse

-- `freeNamesInExpression` and `freeNamesInStatement` returns all of the unbound identifiers that are referenced in
-- the given expression or statement.

freeNamesInExpression :: Ord a => Expression a -> S.Set (Id a)
freeNamesInExpression (VarRef _ i) =
	S.singleton i
freeNamesInExpression (FuncExpr _ _ args body) =
	(S.\\) (freeNamesInScope body) (S.fromList args)
freeNamesInExpression other =
	getConst (traverseExpression freeNamesVisitor other)

freeNamesInStatement :: Ord a => Statement a -> S.Set (Id a)
freeNamesInStatement (FunctionStmt _ _ args body) =
	(S.\\) (freeNamesInScope body) (S.fromList args)
freeNamesInStatement (TryStmt _ body (Just (CatchClause _ i catch)) finally) =
	freeNamesInStatement body
	`S.union` S.delete i (freeNamesInStatement catch)
	`S.union` maybe S.empty freeNamesInStatement finally
freeNamesInStatement other =
	getConst (traverseStatement freeNamesVisitor other)

freeNamesInScope :: Ord a => [Statement a] -> S.Set (Id a)
freeNamesInScope scope =
	(S.\\) (S.unions (map freeNamesInStatement scope)) (S.unions (map varDeclsInStatement scope))

freeNamesVisitor :: Ord a => Visitor (Const (S.Set (Id a))) a
freeNamesVisitor = (defaultVisitor freeNamesVisitor) {
	visitExpression = Const . freeNamesInExpression,
	visitStatement = Const . freeNamesInStatement
	}

varDeclsInStatement :: Ord a => Statement a -> S.Set (Id a)
varDeclsInStatement (VarDeclStmt _ vars) =
	S.fromList [i | VarDecl _ i _ <- vars]
varDeclsInStatement (ForInStmt _ (ForInVar i) _ body) =
	S.insert i (varDeclsInStatement body)
varDeclsInStatement (ForStmt _ (VarInit vars) _ _ body) =
	S.fromList [i | VarDecl _ i _ <- vars] `S.union` varDeclsInStatement body
varDeclsInStatement (FunctionStmt _ _ _ _) =
	S.empty
varDeclsInStatement other =
	getConst (traverseStatement varDeclsVisitor other)

varDeclsVisitor :: Ord a => Visitor (Const (S.Set (Id a))) a
varDeclsVisitor = (defaultVisitor varDeclsVisitor) {
	visitExpression = const (Const S.empty),
	visitStatement = Const . varDeclsInStatement
	}

