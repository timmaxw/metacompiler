module Metacompiler.SLRuntime.Substitute where

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

data TypeSub f = TypeSub {
	performTypeSub :: [Type] -> f Type
	}

simpleTypeSub :: Applicative f => Type -> TypeSub f
simpleTypeSub type_ = TypeSub (pure . foldl TypeApp type_)

data TermSub f = TermSub {
	varsOfTermSub :: S.Set NameOfTerm,
	performTermSub :: [Term] -> f Term
	}

simpleTermSub :: Applicative f => Term -> TermSub f
simpleTermSub term = TermSub (freeVarsAndGlobalsInTerm term) (pure . foldl TermApp term)

substituteType :: Applicative f => M.Map NameOfType (TypeSub f) -> Type -> f Type
substituteType subs ty = substituteType' subs ty []

substituteType' :: Applicative f => M.Map NameOfType (TypeSub f) -> Type -> [Type] -> f Type
substituteType' subs (TypeName n k) args | n `M.elem` subs =
	performTypeSub (subs M.! n) args
substituteType' subs (TypeApp f x) args =
	substituteType' subs f (substituteType' x : args)
substituteType' subs other args@(_:_) =
	foldl TypeApp <$> substituteType' subs other [] <*> pure args
substituteType' subs other [] =
	traverseType (substituteTypeVisitor subs) other

substituteTypeVisitor :: Applicative f => M.Map NameOfType (TypeSub f) -> TypeVisitor f
substituteTypeVisitor subs = TypeVisitor {
	visitType = substituteType subs
	}

substituteTerm :: Applicative f
                 => (M.Map NameOfType (TypeSub f), M.Map NameOfTerm (TermSub f))
                 -> Term -> f Term
substituteTerm subs te = substituteTerm' subs te []

substituteTerm' :: Applicative f
                  => (M.Map NameOfType (TypeSub f), M.Map NameOfTerm (TermSub f))
                  -> Term -> [Term] -> f Term
substituteTerm' (typeSubs, termSubs) (TermName n t) args | n `M.elem` termSubs =
	performTermSub (termSubs M.! n) args
substituteTerm' subs (TermApp f x) args =
	substituteTerm' subs f (substituteTerm' x : args)
substituteTerm' (typeSubs, termSubs) (TermAbs (argName, argType) body) [] = let
	argType' = substituteType typeSubs argType
	(argName', termSubs') = prepareBinding (argName, argType') (freeVarsAndGlobalsInTerm body) termSubs
	body' = substituteTerm (typeSubs, termSubs') body
	in TermAbs <$> ((argName',) <$> argType') <*> body'
substituteTerm' (typeSubs, termSubs) (TermCase subject clauses) [] = let
	subject' = substituteTerm (typeSubs, termSubs) subject
	clauses' = sequenceA [let
		tps' = traverse (substituteType typeSubs) tps
		fieldTypes' = map ($ tps') (fieldTypesOfSLCtorDefn ctor)
		f :: M.Map NameOfTerm (TermSub f) -> [(NameOfTerm, Type)] -> f ([NameOfTerm], Term)
		f subs [] = (,) [] <$> substituteTerm (typeSubs, subs) body
		f subs ((fieldName, fieldType'):fieldNamesAndTypes) = let
			(fieldName', subs') = prepareBinding
				(fieldName, fieldType')
				(freeVarsAndGlobalsInTerm body S.\\ S.fromList (map fst fieldNamesAndTypes))
				subs
			rest = f subs' fieldNamesAndTypes
			in (\ (fieldNames', body') -> (fieldName' : fieldNames', body')) <$> rest
		in (\ tps' (fieldNames', body') -> (ctor, tps', fieldNames', body')) <$> tps' <*> f termSubs fieldNames
		| (ctor, tps, fieldNames, body) <- clauses]
	in TermCase <$> subject' <*> clauses'
substituteTerm' subs other args@(_:_) =
	foldl TermApp <$> substituteTerm' subs other [] <*> pure args
substituteTerm' subs other [] =
	traverseTerm (substituteTermVisitor subs) other

substituteTermVisitor :: Applicative f => (M.Map NameOfType (TypeSub f), M.Map NameOfTerm (TermSub f)) -> TermVisitor f
substituteTermVisitor subs@(typeSubs, _) = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitType = substituteType typeSubs
		},
	visitTerm = substituteTerm subs
	}

performBinding :: (NameOfTerm, Type)
               -> S.Set NameOfTerm
               -> M.Map NameOfTerm (TermSub f)
               -> (NameOfTerm, M.Map NameOfTerm (TermSub f))
performBinding (name, nameType) namesWithin subs = let
	forbiddenNames =
		S.unions [maybe S.empty varsOfTermSub $ M.lookup n subs
			| n <- S.toList (S.delete name namesWithin)]
		`S.union` S.delete name namesWithin
	candidateNames = [NameOfTerm (unNameOfTerm name ++ replicate i '\'') | i <- [0..]]
	Just name' = find (`S.notMember` forbiddenNames) candidateNames
	subs' = if name' == name then S.delete name subs else S.insert name (TermName name' nameType) subs'
	in (name', subs')

