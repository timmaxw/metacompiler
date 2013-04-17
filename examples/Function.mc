
(js-expr-type FunctionType
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		=
	(spec (sl-type "fun a -> r" (type "a" = aSL) (type "r" = rSL)))
)

(let FunctionLambda
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(bodySL :: fun (xSL :: sl-term aSL) -> sl-term rSL)
		(bodyJS :: fun (xSL :: sl-term aSL) (xJS :: js-expr aJS xSL) -> js-expr rJS (bodySL xSL))
		= (js-expr
	(type (FunctionType aSL aJS rSL rJS))
	(spec (sl-term [[\ (x :: a) -> body . x]]
		(type "a" = aSL)
		(term "body" (xSL :: sl-term aSL) = bodySL xSL)
	))
	(impl "function (x) { return body(x); }"
		(expr "body" (xSL :: sl-term aSL | xJS :: js-expr aJS xSL) = bodyJS xSL xJS)
	)
))

(let FunctionApply
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(funSL :: sl-term (sl-type "fun a -> r" (type "a" = aSL) (type "r" = rSL)))
		(funJS :: js-expr (FunctionType aSL aJS rSL rJS) funSL)
		(argSL :: sl-term aSL)
		(argJS :: js-expr aJS argSL)
		= (js-expr
	(type rJS)
	(spec (sl-term "fun arg" (term "fun" = funSL) (term "arg" = argSL)))
	(impl "fun()(arg())" (expr "fun" = funJS) (expr "arg" = argJS))
))
