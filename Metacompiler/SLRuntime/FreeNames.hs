module Metacompiler.Runtime.FreeNames where

import Control.Applicative
import Data.Monoid
import qualified Data.Map as M
import qualified Data.Set as S
import Metacompiler.SLRuntime.Traverse
import Metacompiler.SLRuntime.Types

freeVarsInSLType :: SLType -> M.Map NameOfSLType SLKind
freeVarsInSLType (SLTypeName name kind) = M.singleton name kind
freeVarsInSLType other = getConst (traverseSLType freeTypeVarsVisitor other)

freeVarsInSLTerm :: SLTerm -> (M.Map NameOfSLType SLKind, M.Map NameOfSLTerm SLType)
freeVarsInSLTerm (SLTermName name type_) = (freeVarsInSLType type_, M.singleton name type_)
freeVarsInSLTerm (SLTermAbs (argName, argType) body) = let
	(bodyTypeVars, bodyTermVars) = freeVarsInSLTerm body
	typeVars = bodyTypeVars `M.union` freeVarsInSLType argType
	termVars = M.delete argName bodyTermVars
	in (typeVars, termVars)
freeVarsInSLTerm (SLTermCase subject clauses) = let
	subjectVars = freeVarsInSLTerm subject
	clauseVars = mconcat [let
		(bodyTypeVars, bodyTermVars) = freeVarsInSLTerm body
		typeVars = bodyTypeVars `M.union` M.unions (map freeVarsInSLType tps)
		termVars = foldr M.delete bodyTermVars fns
		in (typeVars, termVars)
		| (_, tps, fns, body) <- clauses]
	in subjectVars `mappend` clauseVars 
freeVarsInSLTerm other = getConst (traverseSLTerm freeAllVarsVisitor other)

freeTypeVarsVisitor :: TypeVisitor (Const (M.Map NameOfSLType SLKind))
freeTypeVarsVisitor = TypeVisitor {
	visitSLType = Const . freeVarsInSLType
	}

freeAllVarsVisitor :: TermVisitor (Const (M.Map NameOfSLType SLKind, M.Map NameOfSLTerm SLType))
freeAllVarsVisitor = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitSLType = \t -> Const (freeVarsInSLType t, M.empty),
		},
	visitSLTerm = Const . freeVarsInSLTerm
	}

globalsInSLType :: SLType -> S.Set NameOfSLType
globalsInSLType (SLTypeDefined defn) =
	S.singleton (nameOfSLDataDefn defn)
globalsInSLType other =
	getConst (traverseSLType typeGlobalsVisitor other)

globalsInSLTerm :: SLTerm -> (S.Set NameOfSLType, S.Set NameOfSLTerm)
globalsInSLTerm (SLTermDefined defn params) =
	(S.singleton (nameOfSLTermDefn defn), S.unions (map globalsInSLType params))
globalsInSLTerm other =
	getConst (traverseSLTerm allGlobalsVisitor other)

typeGlobalsVisitor :: TypeVisitor (Const (S.Set NameOfSLType))
typeGlobalsVisitor = TypeVisitor {
	visitType = Const . globalsInSLType
	}

allGlobalsVisitor :: TermVisitor (Const (S.Set NameOfSLType, S.Set NameOfSLTerm))
allGlobalsVisitor = TermVisitor {
	getTypeVisitor = TypeVisitor {
		visitSLType = \t -> Const (globalsInSLType t, S.empty)
		},
	visitSLTerm = Const . globalsInSLTerm
	}

freeVarsAndGlobalsInSLType :: SLType -> S.Set NameOfSLType
freeVarsAndGlobalsInSLType t =
	S.fromList (M.keys (freeVarsInSLType t)) `S.union` globalsInSLType t

freeVarsAndGlobalsInSLTerm :: SLTerm -> (S.Set NameOfSLType, S.Set NameOfSLTerm)
freeVarsAndGlobalsInSLTerm t = let
	(typeVars, termVars) = freeVarsInSLTerm t
	(typeGlobals, termGlobals) = globalsInSLTerm t
	in (S.fromList (M.keys typeVars) `S.union` typeGlobals, S.fromList (M.keys termVars) `S.union` termGlobals)

