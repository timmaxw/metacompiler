(sl-code [[

	(let countDownToOne . (a :: Nat) :: (List Nat) = case a of
		(Zero .) -> (Nil Nat .)
		(Succ . a') -> (Cons Nat . a' (countDownToOne . a'))
	)
]])

(js-expr-type CountDownList =
	(spec (sl-type "List Nat"))
)

(let CountDownListNil = (js-expr
	(type CountDownList)
	(spec (sl-term "Nil Nat ."))
	(impl "0")
))

(let CountDownListCountDownToOne
		(aSL :: sl-term (sl-type "Nat")) (aJS :: js-expr NatAsNumber aSL)
		= (js-expr
	(type CountDownList)
	(spec (sl-term "countDownToOne a" (term "a" = aSL)))
	(impl "a()" (expr "a" = aJS))
))

(let CountDownListCase
		(subjectSL :: sl-term (sl-type "List Nat"))
		(subjectJS :: js-expr CountDownList subjectSL)
		(resTypeSL :: sl-type "*")
		(resTypeJS :: js-expr-type resTypeSL)
		(nilClauseSL :: sl-term resTypeSL)
		(nilClauseJS :: js-expr resTypeJS nilClauseSL)
		(consClauseSL :: fun
			(xSL :: sl-term (sl-type "Nat"))
			(xsSL :: sl-term (sl-type "List Nat"))
			-> sl-term resTypeSL)
		(consClauseJS :: fun
			(xSL :: sl-term (sl-type "Nat"))
			(xJS :: js-expr NatAsNumber xSL)
			(xsSL :: sl-term (sl-type "List Nat"))
			(xsJS :: js-expr CountDownList xsSL)
			-> js-expr resTypeJS (consClauseSL xSL xsSL))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term "case subject of (Nil Nat .) -> (nc .) (Cons Nat . x xs) -> (cc . x xs)"
		(term "subject" = subjectSL)
		(term "nc" = nilClauseSL)
		(term "cc"
			(xSL :: sl-term (sl-type "Nat"))
			(xsSL :: sl-term (sl-type "List Nat"))
			= consClauseSL xSL xsSL)
	))
	(impl [[
		(function(s) {
			if (s == 0) {
				return nc;
			} else {
				return cc(s, s-1);
			}
		})(subject)
		]]
		(expr "subject" = subjectJS)
		(expr "nc" = nilClauseJS)
		(expr "cc"
			(xSL :: sl-term (sl-type "Nat") | xJS :: js-expr NatAsNumber xSL)
			(xsSL :: sl-term (sl-type "List Nat") | xsJS :: js-expr CountDownList xsSL)
			= consClauseJS xSL xJS xsSL xsJS)
	)
))
