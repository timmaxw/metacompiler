module Metacompiler.TL.FreeNames where

import Control.Applicative
import qualified Data.Set as S
import Metacompiler.TL.Syntax
import Metacompiler.TL.Traverse

-- `freeNamesIn*` finds all unbound TL names in the given object. If the initial `Bool` parameter is set to `False`,
-- then it will ignore unbound names that occur within the contents of a `MOJSExprLoopBreak`.

freeNamesInDirective :: Bool -> Directive a -> S.Set Name
freeNamesInDirective lbs (DLet _ _ ps ty val) = freeNamesInAbstraction lbs ps
	(maybe S.empty (freeNamesInMetaType lbs) ty `S.union` freeNamesInMetaObject lbs val)
freeNamesInDirective lbs (DSLCode _ _) = S.empty
freeNamesInDirective lbs (DJSExprType _ _ ps equiv) = freeNamesInAbstraction lbs ps (freeNamesInMetaObject lbs equiv)
freeNamesInDirective lbs (DJSEmit _ _ bs) = S.unions (map (freeNamesInBinding lbs) bs)

freeNamesInMetaType :: Bool -> MetaType a -> S.Set Name
freeNamesInMetaType lbs (MTFun _ ps r) = freeNamesInAbstraction lbs ps (freeNamesInMetaType lbs r)
freeNamesInMetaType lbs other = getConst (traverseMetaType (freeNamesVisitor lbs) other)

freeNamesInMetaObject :: Bool -> MetaObject a -> S.Set Name
freeNamesInMetaObject lbs (MOAbs _ ps r) = freeNamesInAbstraction lbs ps (freeNamesInMetaObject lbs r)
freeNamesInMetaObject lbs (MOName _ name) = S.singleton name
freeNamesInMetaObject lbs (MOSLTypeLiteral _ _ bs) = S.unions (map (freeNamesInBinding lbs) bs)
freeNamesInMetaObject lbs (MOSLTermLiteral _ _ tybs tebs) =
	S.unions (map (freeNamesInBinding lbs) tybs) `S.union` S.unions (map (freeNamesInBinding lbs) tebs)
freeNamesInMetaObject lbs (MOJSExprLiteral _ equiv type_ _ bs) =
	freeNamesInMetaObject lbs equiv `S.union` freeNamesInMetaObject lbs type_ `S.union`
	S.unions (map (freeNamesInBinding lbs) bs)
freeNamesInMetaObject lbs (MOJSExprLoopBreak _ equiv type_ content) =
	freeNamesInMetaObject lbs equiv `S.union` freeNamesInMetaObject lbs type_ `S.union`
	if lbs then freeNamesInMetaObject lbs content else S.empty
freeNamesInMetaObject lbs other = getConst (traverseMetaObject (freeNamesVisitor lbs) other)

freeNamesInAbstraction :: Bool -> [(Name, MetaType a)] -> S.Set Name -> S.Set Name
freeNamesInAbstraction lbs [] base = base
freeNamesInAbstraction lbs ((pn, pt):rest) base =
	freeNamesInMetaType lbs pt `S.union` S.delete pn (freeNamesInAbstraction lbs rest base)

freeNamesInBinding :: Bool -> Binding a name -> S.Set Name
freeNamesInBinding lbs (Binding _ name ps value) =
	S.unions [freeNamesInAbstraction lbs ps' S.empty | BindingParam _ ps' <- ps]
	`S.union` foldr S.delete (freeNamesInMetaObject lbs value) (concat [map fst ps' | BindingParam _ ps' <- ps])

freeNamesVisitor :: Bool -> Visitor (Const (S.Set Name)) a
freeNamesVisitor lbs = (defaultVisitor (freeNamesVisitor lbs)) {
	visitMetaType = Const . freeNamesInMetaType lbs,
	visitMetaObject = Const . freeNamesInMetaObject lbs
	}
