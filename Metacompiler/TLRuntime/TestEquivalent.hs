module Metacompiler.TLRuntime.TestEquivalent where

import qualified Data.Set as S
import Data.Foldable (all)
import qualified Data.Map as M
import Data.Monoid
import qualified Metacompiler.SLRuntime.Types as SLR
import Metacompiler.TLRuntime.Reduce
import Metacompiler.TLRuntime.Substitute
import Metacompiler.TLRuntime.Types
import Prelude hiding (all)

-- `equivalentMetaTypes` and `equivalentMetaObjects` return `True` if the given meta-types or meta-objects are provably
-- equivalent under all values of all variables, and `False` otherwise.

equivalentMetaTypes :: MetaType -> MetaType -> Bool
equivalentMetaTypes t1 t2 = equivalentMetaTypes' S.empty (reduceMetaType t1) (reduceMetaType t2)

equivalentMetaObjects :: MetaObject -> MetaObject -> Bool
equivalentMetaObjects o1 o2 = equivalentMetaObjects' S.empty (reduceMetaObject o1) (reduceMetaObject o2)

equivalentMetaTypes' :: S.Set (NameOfMetaObject, NameOfMetaObject)
                     -> MetaType
                     -> MetaType
                     -> Bool
equivalentMetaTypes' equivs (MTFun (name1, paramType1) retType1) (MTFun (name2, paramType2) retType2) =
	equivalentMetaTypes' equivs paramType1 paramType2
	&& equivalentMetaTypes' (addEquivalentPair (name1, name2) equivs) retType1 retType2
equivalentMetaTypes' _ (MTSLType k1) (MTSLType k2) =
	k1 == k2
equivalentMetaTypes' equivs (MTSLTerm t1) (MTSLTerm t2) =
	equivalentMetaObjects' equivs t1 t2
equivalentMetaTypes' equivs (MTJSExprType e1) (MTJSExprType e2) =
	equivalentMetaObjects' equivs e1 e2
equivalentMetaTypes' equivs (MTJSExpr t1 e1) (MTJSExpr t2 e2) =
	equivalentMetaObjects' equivs t1 t2 && equivalentMetaObjects' equivs e1 e2
equivalentMetaTypes' equivs _ _ =
	False

equivalentMetaObjects' :: S.Set (NameOfMetaObject, NameOfMetaObject)
                       -> MetaObject
                       -> MetaObject
                       -> Bool
equivalentMetaObjects' equivs (MOApp fun1 arg1) (MOApp fun2 arg2) =
	equivalentMetaObjects' equivs fun1 fun2 && equivalentMetaObjects' equivs arg1 arg2
equivalentMetaObjects' equivs (MOAbs (name1, paramType1) body1) (MOAbs (name2, paramType2) body2) =
	equivalentMetaTypes' equivs paramType1 paramType2
	&& equivalentMetaObjects' (addEquivalentPair (name1, name2) equivs) body1 body2
equivalentMetaObjects' equivs (MOName name1 _) (MOName name2 _) =
	(name1, name2) `S.member` equivs || name1 == name2 && all (\(n1, n2) -> n1 /= name1 && n2 /= name2) equivs
equivalentMetaObjects' equivs (MOSLType type1 tybs1) (MOSLType type2 tybs2) =
	equivalentSLTypes equivs (tybs1, tybs2) type1 type2
equivalentMetaObjects' equivs (MOSLTerm term1 tybs1 tebs1) (MOSLTerm term2 tybs2 tebs2) =
	equivalentSLTerms equivs (tybs1, tybs2) (tebs1, tebs2) S.empty term1 [] term2 []
equivalentMetaObjects' equivs (MOJSExprTypeDefn defn1 params1) (MOJSExprTypeDefn defn2 params2) =
	nameOfJSExprTypeDefn defn1 == nameOfJSExprTypeDefn defn2
	&& and (zipWith (equivalentMetaObjects' equivs) params1 params2)
equivalentMetaObjects' _ _ _ =
	False

equivalentSLTypes :: S.Set (NameOfMetaObject, NameOfMetaObject)
                  -> (M.Map SLR.NameOfType SLTypeBinding, M.Map SLR.NameOfType SLTypeBinding)
                  -> SLR.Type -> SLR.Type
                  -> Bool
equivalentSLTypes tlEquivs (tybs1, tybs2) (SLR.TypeName name1 _) (SLR.TypeName name2 _)
	| name1 `M.member` tybs1 && name2 `M.member` tybs2 = let
		SLTypeBinding value1 = tybs1 M.! name1
		SLTypeBinding value2 = tybs2 M.! name2
		in equivalentMetaObjects' tlEquivs value1 value2
equivalentSLTypes tlEquivs tybs (SLR.TypeDefined defn1) (SLR.TypeDefined defn2) =
	SLR.nameOfDataDefn defn1 == SLR.nameOfDataDefn defn2
equivalentSLTypes tlEquivs tybs (SLR.TypeApp fun1 arg1) (SLR.TypeApp fun2 arg2) =
	equivalentSLTypes tlEquivs tybs fun1 fun2
	&& equivalentSLTypes tlEquivs tybs arg1 arg2
equivalentSLTypes tlEquivs tybs (SLR.TypeFun arg1 ret1) (SLR.TypeFun arg2 ret2) =
	equivalentSLTypes tlEquivs tybs arg1 arg2
	&& equivalentSLTypes tlEquivs tybs ret1 ret2
equivalentSLTypes tlEquivs tybs (SLR.TypeLazy x1) (SLR.TypeLazy x2) =
	equivalentSLTypes tlEquivs tybs x1 x2
equivalentSLTypes _ _ _ _ =
	False

equivalentSLTerms :: S.Set (NameOfMetaObject, NameOfMetaObject)
                  -> (M.Map SLR.NameOfType SLTypeBinding, M.Map SLR.NameOfType SLTypeBinding)
                  -> (M.Map SLR.NameOfTerm SLTermBinding, M.Map SLR.NameOfTerm SLTermBinding)
                  -> S.Set (SLR.NameOfTerm, SLR.NameOfTerm)
                  -> SLR.Term -> [SLR.Term]
                  -> SLR.Term -> [SLR.Term]
                  -> Bool
equivalentSLTerms tlEquivs (tybs1, tybs2) (tebs1, tebs2) slEquivs
			(SLR.TermName name1 _) args1 (SLR.TermName name2 _) args2
	| name1 `M.member` tebs1 && name2 `M.member` tebs2 = let
		SLTermBinding params1 value1 = tebs1 M.! name1
		SLTermBinding params2 value2 = tebs2 M.! name2
		argsLeft1 = drop (length params1) args1
		argsLeft2 = drop (length params2) args2
		tlNamesTaken = S.unions [S.fromList [a, b] | (a, b) <- S.toList tlEquivs]
		tlNameSupply = filter (`S.notMember` tlNamesTaken) [NameOfMetaObject ("n" ++ show i) | i <- [1..]]
		(tebs1', tebs2', tlEquivs') = mconcat [
			(M.singleton slName1 (SLTermBinding [] (MOName tlName undefined)),
			 M.singleton slName2 (SLTermBinding [] (MOName tlName undefined)),
			 S.singleton (tlName, tlName))
			| ((slName1, slName2), tlName) <- zip (S.toList slEquivs) tlNameSupply]
		subs1 = M.fromList [(paramName, MOSLTerm arg tybs1 (M.union tebs1 tebs1'))
		                   | ((paramName, _), arg) <- zip params1 args1]
		subs2 = M.fromList [(paramName, MOSLTerm arg tybs2 (M.union tebs2 tebs2'))
		                   | ((paramName, _), arg) <- zip params2 args2]
		in length argsLeft1 == length argsLeft2
		   && and (zipWith
		               (\ t1 t2 -> equivalentSLTerms tlEquivs (tybs1, tybs2) (tebs1, tebs2) slEquivs t1 [] t2 [])
		               argsLeft1 argsLeft2)
		   && equivalentMetaObjects' (S.union tlEquivs tlEquivs')
		   	      (substituteMetaObject subs1 value1)
		   	      (substituteMetaObject subs2 value2)
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermApp fun1 arg1) args1 term2 args2 =
	equivalentSLTerms tlEquivs tybs tebs slEquivs fun1 (arg1:args1) term2 args2
equivalentSLTerms tlEquivs tybs tebs slEquivs
			term1 args1 (SLR.TermApp fun2 arg2) args2 =
	equivalentSLTerms tlEquivs tybs tebs slEquivs term1 args1 fun2 (arg2:args2)
equivalentSLTerms tlEquivs tybs tebs slEquivs term1 args1@(_:_) term2 args2@(_:_) =
	equivalentSLTerms tlEquivs tybs tebs slEquivs term1 [] term2 []
	&& length args1 == length args2
	&& and (zipWith (\ t1 t2 -> equivalentSLTerms tlEquivs tybs tebs slEquivs t1 [] t2 []) args1 args2)
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermDefined defn1 typs1) [] (SLR.TermDefined defn2 typs2) [] =
	SLR.nameOfTermDefn defn1 == SLR.nameOfTermDefn defn2
	&& and (zipWith (equivalentSLTypes tlEquivs tybs) typs1 typs2)
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermName name1 _) [] (SLR.TermName name2 _) [] =
	(name1, name2) `S.member` slEquivs
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermAbs (name1, type1) body1) [] (SLR.TermAbs (name2, type2) body2) [] =
	equivalentSLTypes tlEquivs tybs type1 type2
	&& equivalentSLTerms tlEquivs tybs tebs (addEquivalentPair (name1, name2) slEquivs) body1 [] body2 []
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermCase subject1 clauses1) [] (SLR.TermCase subject2 clauses2) [] =
	equivalentSLTerms tlEquivs tybs tebs slEquivs subject1 [] subject2 []
	&& length clauses1 == length clauses2
	&& and [SLR.nameOfCtorDefn c1 == SLR.nameOfCtorDefn c2
		&& and (zipWith (equivalentSLTypes tlEquivs tybs) typs1 typs2)
		&& equivalentSLTerms tlEquivs tybs tebs (foldr addEquivalentPair slEquivs (zip fns1 fns2)) b1 [] b2 []
		| ((c1, typs1, fns1, b1), (c2, typs2, fns2, b2)) <- zip clauses1 clauses2]
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermData ctor1 typs1) [] (SLR.TermData ctor2 typs2) [] =
	SLR.nameOfCtorDefn ctor1 == SLR.nameOfCtorDefn ctor2
	&& and (zipWith (equivalentSLTypes tlEquivs tybs) typs1 typs2)
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermWrap x1) [] (SLR.TermWrap x2) [] =
	equivalentSLTerms tlEquivs tybs tebs slEquivs x1 [] x2 []
equivalentSLTerms tlEquivs tybs tebs slEquivs
			(SLR.TermUnwrap x1) [] (SLR.TermUnwrap x2) [] =
	equivalentSLTerms tlEquivs tybs tebs slEquivs x1 [] x2 []
equivalentSLTerms _ _ _ _ _ _ _ _ =
	False

addEquivalentPair :: Ord a => (a, a) -> S.Set (a, a) -> S.Set (a, a)
addEquivalentPair (name1, name2) = S.insert (name1, name2) . S.filter (\ (n1, n2) -> n1 /= name1 && n2 /= name2)

