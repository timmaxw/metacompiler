module Metacompiler.TLSyntax.FreeNames where

import Control.Applicative
import qualified Data.Set as S
import Metacompiler.TLSyntax.Types
import Metacompiler.TLSyntax.Traverse

-- `freeNamesIn*` finds all unbound TL names in the given object.

freeNamesInMetaType :: MetaType a -> S.Set Name
freeNamesInMetaType (MTFun _ ps r) = freeNamesInAbstraction ps (freeNamesInMetaType r)
freeNamesInMetaType other = getConst (traverseMetaType (freeNamesVisitor) other)

freeNamesInMetaObject :: MetaObject a -> S.Set Name
freeNamesInMetaObject (MOAbs _ ps r) = freeNamesInAbstraction ps (freeNamesInMetaObject r)
freeNamesInMetaObject (MOName _ name) = S.singleton name
freeNamesInMetaObject (MOSLTypeLiteral _ _ bs) = S.unions (map (freeNamesInBinding) bs)
freeNamesInMetaObject (MOSLTermLiteral _ _ tybs tebs) =
	S.unions (map (freeNamesInBinding) tybs) `S.union` S.unions (map (freeNamesInBinding) tebs)
freeNamesInMetaObject (MOJSExprLiteral _ equiv type_ _ bs) =
	freeNamesInMetaObject equiv `S.union` freeNamesInMetaObject type_ `S.union`
	S.unions (map (freeNamesInBinding) bs)
freeNamesInMetaObject other = getConst (traverseMetaObject (freeNamesVisitor) other)

freeNamesInAbstraction :: [(Name, MetaType a)] -> S.Set Name -> S.Set Name
freeNamesInAbstraction [] base = base
freeNamesInAbstraction ((pn, pt):rest) base =
	freeNamesInMetaType pt `S.union` S.delete pn (freeNamesInAbstraction rest base)

freeNamesInBinding :: Binding a name -> S.Set Name
freeNamesInBinding (Binding _ name ps value) =
	S.unions [freeNamesInAbstraction ps' S.empty | BindingParam _ ps' <- ps]
	`S.union` foldr S.delete (freeNamesInMetaObject value) (concat [map fst ps' | BindingParam _ ps' <- ps])

freeNamesVisitor :: Visitor (Const (S.Set Name)) a
freeNamesVisitor = (defaultVisitor (freeNamesVisitor)) {
	visitMetaType = Const . freeNamesInMetaType,
	visitMetaObject = Const . freeNamesInMetaObject
	}
