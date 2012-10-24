module SLParser where

import Metacompiler.ParseSExpr
import Metacompiler.SExprToSL

test a = Metacompiler.ParseSExpr.parse a >>= parseSLTermFromSExprs

