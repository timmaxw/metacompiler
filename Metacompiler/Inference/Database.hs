module Metacompiler.Inference.Database where

data ExprPattern = ExprPattern {
	paramsOfExprPattern :: [(Name, R.MetaType)],
	typeOfExprPattern :: R.MetaObject,
	specOfExprPattern :: R.MetaObject,
	valueOfExprPattern :: M.Map Name R.MetaObject -> JS.Expression ()
	}

