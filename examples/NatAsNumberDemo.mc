(js-emit "two = x();"
	(expr "x" = (NatAsNumberPlus
		(sl-term "Succ Zero") (NatAsNumberSucc (sl-term "Zero") NatAsNumberZero)
		(sl-term "Succ Zero") (NatAsNumberSucc (sl-term "Zero") NatAsNumberZero)
		))
	)

