module Metacompiler.Runtime.TestEquivalent where

import qualified Data.Set as S
import Data.Foldable (all)
import Metacompiler.Runtime.Reduce
import Metacompiler.Runtime.Types
import Prelude hiding (all)

-- `equivalentMetaTypes` and `equivalentMetaObjects` return `True` if the given meta-types or meta-objects are provably
-- equivalent under all values of all variables, and `False` otherwise.

equivalentMetaTypes :: MetaType -> MetaType -> Bool
equivalentMetaTypes t1 t2 = equivalentMetaTypes' (S.empty, S.empty) (reduceMetaType t1) (reduceMetaType t2)

equivalentMetaObjects :: MetaObject -> MetaObject -> Bool
equivalentMetaObjects o1 o2 = equivalentMetaObjects' (S.empty, S.empty) (reduceMetaObject o1) (reduceMetaObject o2)

equivalentMetaTypes' :: (S.Set (NameOfMetaObject, NameOfMetaObject), S.Set (NameOfSLTerm, NameOfSLTerm))
                     -> MetaType
                     -> MetaType
                     -> Bool
equivalentMetaTypes' (nameEquivs, nameOfSLTermEquivs) (MTFun (name1, paramType1) retType1) (MTFun (name2, paramType2) retType2) =
	equivalentMetaTypes' (nameEquivs, nameOfSLTermEquivs) paramType1 paramType2
	&& equivalentMetaTypes' (nameEquivs', nameOfSLTermEquivs) retType1 retType2
	where nameEquivs' = S.insert (name1, name2) (S.filter (\(n1, n2) -> n1 /= name1 && n2 /= name2) nameEquivs)
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

equivalentMetaObjects' :: (S.Set (NameOfMetaObject, NameOfMetaObject), S.Set (NameOfSLTerm, NameOfSLTerm))
                       -> MetaObject
                       -> MetaObject
                       -> Bool
equivalentMetaObjects' equivs (MOApp fun1 arg1) (MOApp fun2 arg2) =
	equivalentMetaObjects' equivs fun1 fun2 && equivalentMetaObjects' equivs arg1 arg2
equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) (MOAbs (name1, paramType1) body1) (MOAbs (name2, paramType2) body2) =
	equivalentMetaTypes' (nameEquivs, nameOfSLTermEquivs) paramType1 paramType2
	&& equivalentMetaObjects' (nameEquivs', nameOfSLTermEquivs) body1 body2
	where nameEquivs' = S.insert (name1, name2) (S.filter (\(n1, n2) -> n1 /= name1 && n2 /= name2) nameEquivs)
equivalentMetaObjects' (nameEquivs, _) (MOName name1 _) (MOName name2 _) =
	(name1, name2) `S.member` nameEquivs || name1 == name2 && all (\(n1, n2) -> n1 /= name1 && n2 /= name2) nameEquivs
equivalentMetaObjects' equivs (MOSLTypeDefn defn1) (MOSLTypeDefn defn2) =
	nameOfSLDataDefn defn1 == nameOfSLDataDefn defn2
equivalentMetaObjects' _ (MOSLTypeName name1 _) (MOSLTypeName name2 _) =
	name1 == name2
equivalentMetaObjects' equivs (MOSLTypeApp fun1 arg1) (MOSLTypeApp fun2 arg2) =
	equivalentMetaObjects' equivs fun1 fun2 && equivalentMetaObjects' equivs arg1 arg2
equivalentMetaObjects' equivs (MOSLTypeFun argType1 retType1) (MOSLTypeFun argType2 retType2) =
	equivalentMetaObjects' equivs argType1 argType2 && equivalentMetaObjects' equivs retType1 retType2
equivalentMetaObjects' equivs (MOSLTypeLazy x1) (MOSLTypeLazy x2) =
	equivalentMetaObjects' equivs x1 x2
equivalentMetaObjects' equivs (MOSLTermDefn defn1 typeParams1) (MOSLTermDefn defn2 typeParams2) =
	nameOfSLTermDefn defn1 == nameOfSLTermDefn defn2
	&& all (\(t1, t2) -> equivalentMetaObjects' equivs t1 t2) (zip typeParams1 typeParams2)
equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) (MOSLTermName name1 _) (MOSLTermName name2 _) =
	(name1, name2) `S.member` nameOfSLTermEquivs || name1 == name2 && all (\(n1, n2) -> n1 /= name1 && n2 /= name2) nameOfSLTermEquivs
equivalentMetaObjects' equivs (MOSLTermApp fun1 arg1) (MOSLTermApp fun2 arg2) =
	equivalentMetaObjects' equivs fun1 fun2 && equivalentMetaObjects' equivs arg1 arg2
equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) (MOSLTermAbs (name1, paramType1) body1) (MOSLTermAbs (name2, paramType2) body2) =
	equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) paramType1 paramType2
	&& equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs') body1 body2
	where nameOfSLTermEquivs' = S.insert (name1, name2) (S.filter (\(n1, n2) -> n1 /= name1 && n2 /= name2) nameOfSLTermEquivs)
equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) (MOSLTermCase subject1 clauses1) (MOSLTermCase subject2 clauses2) =
	equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs) subject1 subject2
	&& length clauses1 == length clauses2
	&& all (\((ctor1, ctorTypeParams1, fieldNames1, body1), (ctor2, ctorTypeParams2, fieldNames2, body2)) ->
		nameOfSLCtorDefn ctor1 == nameOfSLCtorDefn ctor2
		&& and (zipWith (equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs)) ctorTypeParams1 ctorTypeParams2)
		&& let
			nameOfSLTermEquivs' = S.filter (\(n1, n2) -> n1 `notElem` fieldNames1 && n2 `notElem` fieldNames2) nameOfSLTermEquivs
			nameOfSLTermEquivs'' = foldr S.insert nameOfSLTermEquivs' (zip fieldNames1 fieldNames2)
			in equivalentMetaObjects' (nameEquivs, nameOfSLTermEquivs'') body1 body2
		) (zip clauses1 clauses2)
equivalentMetaObjects' equivs (MOSLTermData ctor1 typeParams1 fields1) (MOSLTermData ctor2 typeParams2 fields2) =
	nameOfSLCtorDefn ctor1 == nameOfSLCtorDefn ctor2
	&& and (zipWith (equivalentMetaObjects' equivs) typeParams1 typeParams2)
	&& and (zipWith (equivalentMetaObjects' equivs) fields1 fields2)
equivalentMetaObjects' equivs (MOSLTermWrap x1) (MOSLTermWrap x2) =
	equivalentMetaObjects' equivs x1 x2
equivalentMetaObjects' equivs (MOSLTermUnwrap x1) (MOSLTermUnwrap x2) =
	equivalentMetaObjects' equivs x1 x2
equivalentMetaObjects' equivs (MOJSExprTypeDefn defn1 params1) (MOJSExprTypeDefn defn2 params2) =
	nameOfJSExprTypeDefn defn1 == nameOfJSExprTypeDefn defn2
	&& and (zipWith (equivalentMetaObjects' equivs) params1 params2)
equivalentMetaObjects' _ _ _ =
	False

