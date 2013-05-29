module Metacompiler.Inference.Database where

data ExprPattern = ExprPattern {
	paramsOfExprPattern :: [(Name, TLR.MetaType)],
	typeOfExprPattern :: TLR.MetaObject,
	specOfExprPattern :: SLR.Term,
	valueOfExprPattern :: M.Map Name R.MetaObject -> JS.Expression ()
	}

