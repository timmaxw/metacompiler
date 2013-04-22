
(js-expr-type LazyAsFunction
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		=
	(spec (sl-type "lazy a" (type "a" = aSL)))
)

(let LazyAsFunctionWrap
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term aSL) (xJS :: js-expr aJS xSL)
		= (js-expr
	(type (LazyAsFunction aSL aJS))
	(spec (sl-term "wrap x" (term "x" = xSL)))
	(impl "function () { return x(); }" (expr "x" = xJS))
))

(let LazyAsFunctionUnwrap
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term (sl-type "lazy a" (type "a" = aSL))) (xJS :: js-expr (LazyAsFunction aSL aJS) xSL)
		= (js-expr
	(type aJS)
	(spec (sl-term "unwrap x" (term "x" = xSL)))
	(impl "x()()" (expr "x" = xJS))
))

