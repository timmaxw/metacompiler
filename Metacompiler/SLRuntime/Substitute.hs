module Metacompiler.SLRuntime.Substitute where

import Control.Monad.Identity
import Data.List
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S
import qualified Metacompiler.JS as JS
import Metacompiler.SLRuntime.FreeNames
import Metacompiler.SLRuntime.Traverse
import Metacompiler.SLRuntime.Types

{- `substituteSLType` and `substituteSLTerm` recursively traverse the given object, looking for variables that appear
in the given maps. When a variable is found, it is replaced with the result of `performTypeSub` or `performTermSub`,
where the argument to `performTypeSub` or `performTermSub` is a list of the types or terms that are being passed as
parameters to the variable.

In addition, the entire thing is parameterized on an applicative functor, which is useful if you want to do something
other than simple substitution (e.g. count the number of times a variable is used).

This is best explained by example. Suppose that we run:
	substituteSLTerm (M.singleton (NameOfSLTerm "f") termSub, M.empty) term
where `term` is the SL term `(f x y)`. Then `performTermSub termSub` will be called, with the parameter:
	[SLTermName (NameOfSLTerm "x") ..., SLTermName (NameOfSLTerm "y") ...]
Whatever `performTermSub termSub` returns will be the return value of `substituteSLTerm`.

`substituteSLTerm` also correctly preserves semantics. There are two tricky cases:

1. In the expression `f x (\ x :: T -> x)`, substituting `x` should only affect the first parameter to `f` and not the
    second, because the `x`s in the second parameter are semantically a different variable.

2. In the expression `(\ x :: T -> f x y)`, if `y` is to be replaced with `g x`, then the result should be
    `(\ x' :: T -> f x' (g x))`. This is because any variables referenced in the substitution are assumed to be
    interpreted in the scope of outside of the expression. This is also called "capture-avoiding substitution". To
    facilitate this, every `TermSub` has an extra field `varsOfTermSub`, which is a set containing all of the
    variables that the `TermSub` might possibly introduce into scope.
-}

data TypeSub f = TypeSub {
	performTypeSub :: [SLType] -> f SLType
	}

data TermSub f = TermSub {
	varsOfTermSub :: S.Set NameOfSLTerm,
	performTermSub :: [SLTerm] -> f SLTerm
	}

substituteSLType :: Applicative f => M.Map NameOfSLType (TypeSub f) -> SLType -> f SLType
substituteSLType subs ty = substituteSLType' subs ty []

substituteSLType' :: Applicative f => M.Map NameOfSLType (TypeSub f) -> SLType -> [SLType] -> f SLType
substituteSLType' subs (SLTypeName n k) args | n `M.elem` subs =
	performTypeSub (subs M.! n) args
substituteSLType' subs (SLTypeApp f x) args =
	substituteSLType' subs f (substituteSLType' x : args)
substituteSLType' subs other args@(_:_) =
	foldl SLTypeApp <$> substituteSLType' subs other [] <*> pure args
substituteSLType' subs other [] =
	traverseSLType (substituteTypeVisitor subs) other

substituteTypeVisitor :: Applicative f => M.Map NameOfSLType (TypeSub f) -> TypeVisitor f
substituteTypeVisitor subs = TypeVisitor {
	visitSLType = substituteSLType subs
	}

substituteSLTerm :: Applicative f
                 => (M.Map NameOfSLType (TypeSub f), M.Map NameOfSLTerm (TermSub f))
                 -> SLTerm -> f SLTerm
substituteSLTerm subs te = substituteSLTerm' subs te []

substituteSLTerm' :: Applicative f
                  => (M.Map NameOfSLType (TypeSub f), M.Map NameOfSLTerm (TermSub f))
                  -> SLTerm -> [SLTerm] -> f SLTerm
substituteSLTerm' (typeSubs, termSubs) (SLTermName n t) args | n `M.elem` termSubs =
	performTermSub (termSubs M.! n) args
substituteSLTerm' subs (SLTermApp f x) args =
	substituteSLTerm' subs f (substituteSLTerm' x : args)
substituteSLTerm' (typeSubs, termSubs) (SLTermAbs (argName, argType) body) [] = let
	argType' = substituteSLType typeSubs argType
	(argName', termSubs') = prepareBinding (argName, argType') (freeVarsAndGlobalsInSLTerm body) termSubs
	body' = substituteSLTerm (typeSubs, termSubs') body
	in SLTermAbs <$> ((argName',) <$> argType') <*> body'
substituteSLTerm' (typeSubs, termSubs) (SLTermCase subject clauses) [] = let
	subject' = substituteSLTerm (typeSubs, termSubs) subject
	clauses' = sequenceA [let
		tps' = traverse (substituteSLType typeSubs) tps
		fieldTypes' = map ($ tps') (fieldTypesOfSLCtorDefn ctor)
		f :: M.Map NameOfSLTerm (TermSub f) -> [(NameOfSLTerm, SLType)] -> f ([NameOfSLTerm], SLTerm)
		f subs [] = (,) [] <$> substituteSLTerm (typeSubs, subs) body
		f subs ((fieldName, fieldType'):fieldNamesAndTypes) = let
			(fieldName', subs') = prepareBinding
				(fieldName, fieldType')
				(freeVarsAndGlobalsInSLTerm body S.\\ S.fromList (map fst fieldNamesAndTypes))
				subs
			rest = f subs' fieldNamesAndTypes
			in (\ (fieldNames', body') -> (fieldName' : fieldNames', body')) <$> rest
		in (\ tps' (fieldNames', body') -> (ctor, tps', fieldNames', body')) <$> tps' <*> f termSubs fieldNames
		| (ctor, tps, fieldNames, body) <- clauses]
	in SLTermCase <$> subject' <*> clauses'
substituteSLTerm' subs other args@(_:_) =
	foldl SLTermApp <$> substituteSLTerm' subs other [] <*> pure args
substituteSLTerm' subs other [] =
	traverseSLTerm (substituteTermVisitor subs) other

substituteTermVisitor :: Applicative f => (M.Map NameOfSLType (TypeSub f), M.Map NameOfSLTerm (TermSub f)) -> TermVisitor f
substituteTermVisitor subs@(typeSubs, _) = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitSLType = substituteSLType typeSubs
		},
	visitSLTerm = substituteSLTerm subs
	}

performBinding :: (NameOfSLTerm, SLType)
               -> S.Set NameOfSLTerm
               -> M.Map NameOfSLTerm (TermSub f)
               -> (NameOfSLTerm, M.Map NameOfSLTerm (TermSub f))
performBinding (name, nameType) namesWithin subs = let
	forbiddenNames =
		S.unions [maybe S.empty varsOfTermSub $ M.lookup n subs
			| n <- S.toList (S.delete name namesWithin)]
		`S.union` S.delete name namesWithin
	candidateNames = [NameOfSLTerm (unNameOfSLTerm name ++ replicate i '\'') | i <- [0..]]
	Just name' = find (`S.notMember` forbiddenNames) candidateNames
	subs' = if name' == name then S.delete name subs else S.insert name (SLTermName name' nameType) subs'
	in (name', subs')

