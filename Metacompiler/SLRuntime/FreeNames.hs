module Metacompiler.SLRuntime.FreeNames where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Metacompiler.SLRuntime.Traverse
import Metacompiler.SLRuntime.Types

freeVarsInType :: Type -> M.Map NameOfType Kind
freeVarsInType (TypeName name kind) = M.singleton name kind
freeVarsInType other = getConst (traverseType freeTypeVarsVisitor other)

freeVarsInTerm :: Term -> (M.Map NameOfType Kind, M.Map NameOfTerm Type)
freeVarsInTerm (TermName name type_) = (freeVarsInType type_, M.singleton name type_)
freeVarsInTerm (TermAbs (argName, argType) body) = let
	(bodyTypeVars, bodyTermVars) = freeVarsInTerm body
	typeVars = bodyTypeVars `M.union` freeVarsInType argType
	termVars = M.delete argName bodyTermVars
	in (typeVars, termVars)
freeVarsInTerm (TermCase subject clauses) = let
	subjectVars = freeVarsInTerm subject
	clauseVars = mconcat [let
		(bodyTypeVars, bodyTermVars) = freeVarsInTerm body
		typeVars = bodyTypeVars `M.union` M.unions (map freeVarsInType tps)
		termVars = foldr M.delete bodyTermVars fns
		in (typeVars, termVars)
		| (_, tps, fns, body) <- clauses]
	in subjectVars `mappend` clauseVars 
freeVarsInTerm other = getConst (traverseTerm freeAllVarsVisitor other)

freeTypeVarsVisitor :: TypeVisitor (Const (M.Map NameOfType Kind))
freeTypeVarsVisitor = TypeVisitor {
	visitType = Const . freeVarsInType
	}

freeAllVarsVisitor :: TermVisitor (Const (M.Map NameOfType Kind, M.Map NameOfTerm Type))
freeAllVarsVisitor = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitType = \t -> Const (freeVarsInType t, M.empty)
		},
	visitTerm = Const . freeVarsInTerm
	}

globalsInType :: Type -> S.Set NameOfType
globalsInType (TypeDefined defn) =
	S.singleton (nameOfDataDefn defn)
globalsInType other =
	getConst (traverseType typeGlobalsVisitor other)

globalsInTerm :: Term -> (S.Set NameOfType, S.Set NameOfTerm)
globalsInTerm (TermDefined defn params) =
	(S.unions (map globalsInType params), S.singleton (nameOfTermDefn defn))
globalsInTerm other =
	getConst (traverseTerm allGlobalsVisitor other)

typeGlobalsVisitor :: TypeVisitor (Const (S.Set NameOfType))
typeGlobalsVisitor = TypeVisitor {
	visitType = Const . globalsInType
	}

allGlobalsVisitor :: TermVisitor (Const (S.Set NameOfType, S.Set NameOfTerm))
allGlobalsVisitor = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitType = \t -> Const (globalsInType t, S.empty)
		},
	visitTerm = Const . globalsInTerm
	}

freeVarsAndGlobalsInType :: Type -> S.Set NameOfType
freeVarsAndGlobalsInType t =
	S.fromList (M.keys (freeVarsInType t)) `S.union` globalsInType t

freeVarsAndGlobalsInTerm :: Term -> (S.Set NameOfType, S.Set NameOfTerm)
freeVarsAndGlobalsInTerm t = let
	(typeVars, termVars) = freeVarsInTerm t
	(typeGlobals, termGlobals) = globalsInTerm t
	in (S.fromList (M.keys typeVars) `S.union` typeGlobals, S.fromList (M.keys termVars) `S.union` termGlobals)

