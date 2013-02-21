module Metacompiler.SExpr.UtilsTo where

-- This module contains utilities that will be useful when trying to convert from some other format to S-expressions;
-- hence the somewhat strange name `UtilsTo`.

import Metacompiler.Range
import Metacompiler.SExpr.Types

undefinedPoint :: Point
undefinedPoint = Point undefined undefined

undefinedRange :: Range
undefinedRange = Range undefinedPoint undefinedPoint

sExprsFromList :: [SExpr] -> SExprs
sExprsFromList [] = Nil undefinedPoint
sExprsFromList (x:xs) = Cons x (sExprsFromList xs)

mkList :: SExprs -> SExpr
mkList = List undefinedRange

mkList' :: [SExpr] -> SExpr
mkList' = mkList . sExprsFromList

mkAtom :: String -> SExpr
mkAtom = Atom undefinedRange

mkQuoted :: String -> SExpr
mkQuoted = Quoted undefinedRange

