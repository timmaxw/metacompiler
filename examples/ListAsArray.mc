(sl-code [[

	(data List (a :: *) = (Nil) (Cons a (List a)))

	(let length (a :: *) . (l :: List a) :: Nat =
		(case l of
			(Nil a .) -> (Zero .)
			(Cons a . x xs) -> (Succ . (length a . xs))
		)
	)

	(let concat (a :: *) . (left :: List a) (right :: List a) :: (List a) =
		(case left of
			(Nil a .) -> (right)
			(Cons a . x xs) -> (Cons a . x (concat a . xs right))
		)
	)
]])

(js-expr-type ListAsArray (aSL :: sl-type "*") (aJS :: js-expr-type aSL) =
	(spec (sl-type "List a" (type "a" = aSL)))
)

(let ListAsArrayNil
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "Nil a ." (type "a" = aSL)))
	(impl "[]")
))

(let ListAsArrayCons
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term aSL) (xJS :: js-expr aJS xSL)
		(xsSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xsJS :: js-expr (ListAsArray aSL aJS) xsSL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "Cons a . x xs" (type "a" = aSL) (term "x" = xSL) (term "xs" = xsSL)))
	(impl "[x()].concat(xs())" (expr "x" = xJS) (expr "xs" = xsJS))
))

(let ListAsArrayLength
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xJS :: js-expr (ListAsArray aSL aJS) xSL)
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "length a . x" (type "a" = aSL) (term "x" = xSL)))
	(impl "x().length" (expr "x" = xJS))
))

(let ListAsArrayConcat
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xJS :: js-expr (ListAsArray aSL aJS) xSL)
		(ySL :: sl-term (sl-type "List a" (type "a" = aSL))) (yJS :: js-expr (ListAsArray aSL aJS) ySL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "concat a . x y" (type "a" = aSL) (term "x" = xSL) (term "y" = ySL)))
	(impl "x().concat(y())" (expr "x" = xJS) (expr "y" = yJS))
))

