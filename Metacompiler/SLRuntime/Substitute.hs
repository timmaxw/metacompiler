module Metacompiler.SLRuntime.Substitute where

import Control.Applicative
import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import Metacompiler.SLRuntime.FreeNames
import Metacompiler.SLRuntime.Traverse
import Metacompiler.SLRuntime.Types

{- `substituteType` and `substituteTerm` recursively traverse the given object, looking for variables that appear
in the given maps. When a variable is found, it is replaced with the result of `performTypeSub` or `performTermSub`,
where the argument to `performTypeSub` or `performTermSub` is a list of the types or terms that are being passed as
parameters to the variable.

In addition, the entire thing is parameterized on an applicative functor, which is useful if you want to do something
other than simple substitution (e.g. count the number of times a variable is used).

This is best explained by example. Suppose that we run:
	substituteTerm (M.singleton (NameOfTerm "f") termSub, M.empty) term
where `term` is the SL term `(f x y)`. Then `performTermSub termSub` will be called, with the parameter:
	[TermName (NameOfTerm "x") ..., TermName (NameOfTerm "y") ...]
Whatever `performTermSub termSub` returns will be the return value of `substituteTerm`.

`substituteTerm` also correctly preserves semantics. There are two tricky cases:

1. In the expression `f x (\ x :: T -> x)`, substituting `x` should only affect the first parameter to `f` and not the
    second, because the `x`s in the second parameter are semantically a different variable.

2. In the expression `(\ x :: T -> f x y)`, if `y` is to be replaced with `g x`, then the result should be
    `(\ x' :: T -> f x' (g x))`. This is because any variables referenced in the substitution are assumed to be
    interpreted in the scope of outside of the expression. This is also called "capture-avoiding substitution". To
    facilitate this, every `TermSub` has an extra field `varsOfTermSub`, which is a set containing all of the
    variables that the `TermSub` might possibly introduce into scope.
-}

data TypeSub m = TypeSub {
	performTypeSub :: [Type] -> m Type
	}

simpleTypeSub :: Monad m => Type -> TypeSub m
simpleTypeSub type_ = TypeSub (return . foldl TypeApp type_)

data TermSub m = TermSub {
	varsOfTermSub :: S.Set NameOfTerm,
	performTermSub :: [Term] -> m Term
	}

simpleTermSub :: Monad m => Term -> TermSub m
simpleTermSub term = TermSub (snd (freeVarsAndGlobalsInTerm term)) (return . foldl TermApp term)

substituteType :: Monad m => M.Map NameOfType (TypeSub m) -> Type -> m Type
substituteType subs ty = substituteType' subs ty []

substituteType' :: Monad m => M.Map NameOfType (TypeSub m) -> Type -> [Type] -> m Type
substituteType' subs (TypeName n k) args | n `M.member` subs =
	performTypeSub (subs M.! n) args
substituteType' subs (TypeApp f x) args = do
	arg' <- substituteType subs x
	substituteType' subs f (arg' : args)
substituteType' subs other args@(_:_) = do
	other <- substituteType subs other
	return (foldl TypeApp other args)
substituteType' subs other [] =
	unwrapMonad $ traverseType (substituteTypeVisitor subs) other

substituteTypeVisitor :: Monad m
                      => M.Map NameOfType (TypeSub m)
                      -> TypeVisitor (WrappedMonad m)
substituteTypeVisitor subs = TypeVisitor {
	visitType = WrapMonad . substituteType subs
	}

substituteTerm :: Monad m
                 => (M.Map NameOfType (TypeSub m), M.Map NameOfTerm (TermSub m))
                 -> Term -> m Term
substituteTerm subs te = substituteTerm' subs te []

substituteTerm' :: Monad m
                  => (M.Map NameOfType (TypeSub m), M.Map NameOfTerm (TermSub m))
                  -> Term -> [Term] -> m Term
substituteTerm' (typeSubs, termSubs) (TermName n t) args | n `M.member` termSubs =
	performTermSub (termSubs M.! n) args
substituteTerm' subs (TermApp f x) args = do
	x' <- substituteTerm subs x
	substituteTerm' subs f (x' : args)
substituteTerm' (typeSubs, termSubs) (TermAbs (argName, argType) body) [] = do
	argType' <- substituteType typeSubs argType
	let (argName', termSubs') = performBinding (argName, argType') (snd (freeVarsAndGlobalsInTerm body)) termSubs
	body' <- substituteTerm (typeSubs, termSubs') body
	return (TermAbs (argName', argType') body')
substituteTerm' (typeSubs, termSubs) (TermCase subject clauses) [] = do
	subject' <- substituteTerm (typeSubs, termSubs) subject
	clauses' <- sequence [do
		tps' <- mapM (substituteType typeSubs) tps
		let fieldTypes' = map ($ tps') (fieldTypesOfCtorDefn ctor)
		let
			-- f :: M.Map NameOfTerm (TermSub m) -> [(NameOfTerm, Type)] -> m ([NameOfTerm], Term)
			f subs [] = do
				body' <- substituteTerm (typeSubs, subs) body
				return ([], body')
			f subs ((fieldName, fieldType'):fieldNamesAndTypes) = do
				let (fieldName', subs') = performBinding
					(fieldName, fieldType')
					(snd (freeVarsAndGlobalsInTerm body) S.\\ S.fromList (map fst fieldNamesAndTypes))
					subs
				(fieldNames', body') <- f subs' fieldNamesAndTypes
				return (fieldName' : fieldNames', body')
		(fieldNames', body') <- f termSubs (zip fieldNames fieldTypes')
		return (ctor, tps', fieldNames', body')
		| (ctor, tps, fieldNames, body) <- clauses]
	return (TermCase subject' clauses')
substituteTerm' subs other args@(_:_) = do
	other' <- substituteTerm subs other
	return (foldl TermApp other' args)
substituteTerm' subs other [] =
	unwrapMonad $ traverseTerm (substituteTermVisitor subs) other

substituteTermVisitor :: Monad m
                      => (M.Map NameOfType (TypeSub m), M.Map NameOfTerm (TermSub m))
                      -> TermVisitor (WrappedMonad m)
substituteTermVisitor subs@(typeSubs, _) = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitType = WrapMonad . substituteType typeSubs
		},
	visitTerm = WrapMonad . substituteTerm subs
	}

performBinding :: Monad m
               => (NameOfTerm, Type)
               -> S.Set NameOfTerm
               -> M.Map NameOfTerm (TermSub m)
               -> (NameOfTerm, M.Map NameOfTerm (TermSub m))
performBinding (name, nameType) namesWithin subs = let
	forbiddenNames =
		S.unions [maybe S.empty varsOfTermSub $ M.lookup n subs
			| n <- S.toList (S.delete name namesWithin)]
		`S.union` S.delete name namesWithin
	candidateNames = [NameOfTerm (unNameOfTerm name ++ replicate i '\'') | i <- [0..]]
	Just name' = find (`S.notMember` forbiddenNames) candidateNames
	subs' = if name' == name then M.delete name subs else M.insert name (simpleTermSub (TermName name' nameType)) subs
	in (name', subs')

