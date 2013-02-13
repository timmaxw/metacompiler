(sl-code [[

	(data Nat = (Zero) (Succ Nat))

]])

(js-expr-type NatAsNumber =
	(spec (sl-type "Nat"))
)

(let NatAsNumberZero
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "Zero"))
	(impl "0")
))

(let NatAsNumberSucc
		(xSL :: sl-term (sl-type "Nat"))
		(xJS :: js-expr NatAsNumber xSL)
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "Succ x" (term "x" = xSL)))
	(impl "x() + 1" (expr "x" = xJS))
))

(js-emit "alert(x());" (expr "x" = NatAsNumberZero))

