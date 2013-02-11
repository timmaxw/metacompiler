module Metacompiler.JS.Substitute (
	Subst(..), substituteExpression, substituteLValue, substituteStatement, substituteScope
	) where

import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Language.ECMAScript3.Syntax
import Metacompiler.JS.FreeNames
import Metacompiler.JS.Traverse

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
substituteExpression subs (FuncExpr _ maybeName args body) = let
	maybeName' = case maybeName of
		Nothing -> Nothing
		Just name -> Just $ case M.lookup name subs of
			Just (SubstId i') -> i'
			Just _ -> error "variable to be substituted appears in assignment context"
			Nothing -> name
	freeInBody = freeNamesInScope body
	(subs', args') = prepareForBindingNames freeInBody (subs, args)
	body' = substituteScope subs' (M.fromList (zip args args')) body
	in FuncExpr () maybeName' args' body'
substituteExpression subs other =
	runIdentity (traverseExpression (substitutionVisitor subs) other)

substituteLValue :: M.Map (Id ()) Subst -> LValue () -> LValue ()
substituteLValue subs (LVar _ i) = case M.lookup (Id () i) subs of
	Just (SubstValue _) -> error "variable to be substituted appears in assignment context"
	Just (SubstFun _ _) -> error "variable to be substituted appears in assignment context"
	Just (SubstId i') -> LVar () (unId i')
	Nothing -> LVar () i
substituteLValue subs other =
	runIdentity (traverseLValue (substitutionVisitor subs) other)

substitutionVisitor :: M.Map (Id ()) Subst -> Visitor Identity ()
substitutionVisitor subs = Visitor {
	visitExpression = Identity . substituteExpression subs,
	visitLValue = Identity . substituteLValue subs,
	visitStatement = error "we shouldn't get from an expression to a statement without going through FuncExpr"
	}

substituteStatement :: M.Map (Id ()) Subst -> M.Map (Id ()) (Id ()) -> Statement () -> Statement ()
substituteStatement subs declSubs (VarDeclStmt _ vars) = let
	vars' = [let
		i' = case M.lookup i declSubs of
			Just i' -> i'
			Nothing -> i
		v' = fmap (substituteExpression subs) v
		in VarDecl () i' v'
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
		v' = fmap (substituteExpression subs) v
		in VarDecl () i' v'
		| VarDecl _ i v <- vars]
	test' = fmap (substituteExpression subs) test
	step' = fmap (substituteExpression subs) step
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
	visitExpression = Identity . substituteExpression subs,
	visitLValue = Identity . substituteLValue subs,
	visitStatement = Identity . substituteStatement subs declSubs
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
                       -> (M.Map (Id ()) Subst, [Id ()])
                       -> (M.Map (Id ()) Subst, [Id ()])
prepareForBindingNames freeNamesWithin (subs, names) = let
	forbidden = S.unions [
		case M.lookup name subs of
			Just (SubstValue x) -> freeNamesInExpression x
			Just (SubstFun _ x) -> x
			Just (SubstId i) -> S.singleton i
			Nothing -> S.singleton name
		| name <- S.toList ((S.\\) freeNamesWithin (S.fromList names))]
	names' = snd (mapAccumL
		(\ forbidden name -> let
			candidates = [Id () (unId name ++ replicate n '\'') | n <- [0..]]
			Just name' = find (`S.notMember` forbidden) candidates
			in (S.insert name' forbidden, name'))
		forbidden
		names)
	subs' = M.fromList (zip names (map SubstId names'))
		`M.union` M.filterWithKey (\k _ -> k `notElem` names') subs
	in (subs', names')

