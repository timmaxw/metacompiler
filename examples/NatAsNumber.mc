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
	(spec (sl-term "Succ x"))
	(impl "x() + 1" (expr "x" = x))
))

(let NatAsNumberPlus
		(xSL :: sl-term (sl-type "Nat"))
		(xJS :: js-expr NatAsNumber xSL)
		(ySL :: sl-term (sl-type "Nat"))
		(yJS :: js-expr NatAsNumber ySL)
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "plus x y" (term "x" = xSL) (term "y" = ySL)))
	(impl "x() + y()" (expr "x" = xJS) (expr "y" = yJS))
))

(let NatAsNumberTimes
		(xSL :: sl-term (sl-type "Nat"))
		(xJS :: js-expr NatAsNumber xSL)
		(ySL :: sl-term (sl-type "Nat"))
		(yJS :: js-expr NatAsNumber ySL)
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "times x y" (term "x" = xSL) (term "y" = ySL)))
	(impl "x() * y()" (expr "x" = xJS) (expr "y" = yJS))
))

(let NatAsNumberCase
		(subjectSL :: sl-term (sl-type "Nat"))
		(subjectJS :: js-expr NatAsNumber subjectSL)
		(resTypeSL :: sl-type "*")
		(resTypeJS :: js-expr-type resTypeSL)
		(zeroClauseSL :: sl-term resTypeSL)
		(zeroClauseJS :: js-expr resTypeJS zeroClauseSL)
		(succClauseSL :: fun (xSL :: sl-term (sl-type "Nat")) -> sl-term resTypeSL)
		(succClauseJS :: fun (xSL :: sl-term (sl-type "Nat")) (xJS :: js-expr NatAsNumber xSL) -> js-expr resTypeJS (succClauseSL xSL))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term "(case s of (Zero) -> (zc) (Succ x) -> (sc x))"
		(term "s" = subjectSL)
		(term "zc" = zeroClauseSL)
		(term "sc" (xSL :: sl-term (sl-type "Nat")) = succClauseSL xSL)
		))
	(impl
		[[
			(function(s) {
				if (s == 0) { return zc(); }
				else { return sc(s); }
			})(subj())
		]]
		(expr "subj" = subjectJS)
		(expr "zc" = zeroClauseJS)
		(expr "sc" (xSL :: sl-term (sl-type "Nat") | xJS :: js-expr NatAsNumber xSL) = succClauseJS xSL xJS)
		)
))
