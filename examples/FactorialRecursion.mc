(sl-code [[

	(let factorial (x :: Nat) :: Nat =
		(case x of
			(Zero) -> (Succ Zero)
			(Succ y) -> (times x (factorial y))
		)
	)

]])

(let Factorial (xSL :: sl-term (sl-type "Nat")) (xJS :: js-expr NatAsNumber xSL) = (js-loop-break
	(equiv (sl-term "factorial x" (term "x" = xSL)))
	(type NatAsNumber)
	(content
		(NatAsNumberCase
			xSL xJS
			(sl-type "Nat") NatAsNumber
			(sl-term "Succ Zero") (NatAsNumberSucc NatAsNumberZero)
			(\ (ySL :: sl-term (sl-type "Nat")) ->
				sl-term "times x (factorial y)"
				)
			(\ (ySL :: sl-term (sl-type "Nat")) (yJS :: js-expr NatAsNumber ySL) ->
				NatAsNumberTimes
					xSL xJS
					(sl-term "factorial y" (term "y" = ySL)) (Factorial ySL yJS)
				)
			)
		)
	))

(js-emit "the_answer = x;" (expr "x" =
	(Factorial (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc NatAsNumberZero))))))
	))

