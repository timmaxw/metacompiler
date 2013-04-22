
(js-expr-type Function2Type
		(a1SL :: sl-type "*") (a1JS :: js-expr-type a1SL)
		(a2SL :: sl-type "*") (a2JS :: js-expr-type a2SL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		=
	(spec (sl-type "fun a1 a2 -> r"
		(type "a1" = a1SL) (type "a2" = a2SL) (type "r" = rSL)
	))
)

(let FunctionLambda
		(a1SL :: sl-type "*") (a1JS :: js-expr-type a1SL)
		(a2SL :: sl-type "*") (a2JS :: js-expr-type a2SL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(bodySL :: fun
			(x1SL :: sl-term a1SL) (x2SL :: sl-term a2SL)
			-> sl-term rSL)
		(bodyJS :: fun
			(x1SL :: sl-term a1SL) (x1JS :: js-expr a1JS x1SL)
			(x2SL :: sl-term a2SL) (x2JS :: js-expr a2JS x2SL)
			-> js-expr rJS (bodySL x1SL x2SL))
		= (js-expr
	(type (Function2Type a1SL a1JS a2SL a2JS rSL rJS))
	(spec (sl-term [[\ (x1 :: a1) (x2 :: a2) -> body . x1 x2]]
		(type "a1" = a1SL) (type "a2" = a2SL)
		(term "body" (x1SL :: sl-term a1SL) (x2SL :: sl-term a2SL) = bodySL x1SL x2SL)
	))
	(impl "function (x1, x2) { return body(x1, x2); }"
		(expr "body"
			(x1SL :: sl-term a1SL | x1JS :: js-expr a1JS x1SL)
			(x2SL :: sl-term a2SL | x2JS :: js-expr a2JS x2SL)
			= bodyJS x1SL x1JS x2SL x2JS)
	)
))

(let FunctionApply
		(a1SL :: sl-type "*") (a1JS :: js-expr-type a1SL)
		(a2SL :: sl-type "*") (a2JS :: js-expr-type a2SL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(funSL :: sl-term (sl-type "fun a1 a2 -> r" (type "a1" = a1SL) (type "a2" = a2SL) (type "r" = rSL)))
		(funJS :: js-expr (Function2Type a1SL a1JS a2SL a2JS rSL rJS) funSL)
		(arg1SL :: sl-term a1SL)
		(arg1JS :: js-expr a1JS arg1SL)
		(arg2SL :: sl-term a2SL)
		(arg2JS :: js-expr a2JS arg2SL)
		= (js-expr
	(type rJS)
	(spec (sl-term "fun arg1 arg2" (term "fun" = funSL) (term "arg1" = arg1SL) (term "arg2" = arg2SL)))
	(impl "fun()(arg1(), arg2())" (expr "fun" = funJS) (expr "arg1" = arg1JS) (expr "arg2" = arg2JS))
))

