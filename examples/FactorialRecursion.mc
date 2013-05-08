(sl-code [[

	(let factorial . (x :: Nat) :: Nat =
		(case x of
			(Zero .) -> (Succ . Zero)
			(Succ . y) -> (times . x (factorial . y))
		)
	)

]])

(js-expr-global Factorial (xSL :: sl-term (sl-type "Nat")) (xJS :: js-expr NatAsNumber xSL) =
	(args xJS)
	(spec (sl-term "factorial . x" (term "x" = xSL)))
	(type NatAsNumber)
	(body
		(js-expr-convert-spec
			(in-spec
				(sl-term
					"(case x of (Zero .) -> (Succ . Zero) (Succ . y) -> (times . x (factorial . y)))"
					(term "x" = xSL)
					)
				)
			(out-spec
				(sl-term
					"factorial . x"
					(term "x" = xSL)
					)
				)
			(content
				(NatAsNumberCase
					xSL xJS
					(sl-type "Nat") NatAsNumber
					(sl-term "Succ Zero") (NatAsNumberSucc (sl-term "Zero") NatAsNumberZero)
					(\ (ySL :: sl-term (sl-type "Nat")) ->
						sl-term "times . x (factorial . y)" (term "x" = xSL) (term "y" = ySL)
						)
					(\ (ySL :: sl-term (sl-type "Nat")) (yJS :: js-expr NatAsNumber ySL) ->
						NatAsNumberTimes
							xSL xJS
							(sl-term "factorial . y" (term "y" = ySL)) (Factorial ySL yJS)
						)
					)
				)
			)
		)
	)

(js-emit "the_answer = x();" (expr "x" =
	(Factorial
		(sl-term "Succ (Succ (Succ Zero))")
		(NatAsNumberSucc
			(sl-term "Succ (Succ Zero)")
			(NatAsNumberSucc
				(sl-term "Succ Zero")
				(NatAsNumberSucc
					(sl-term "Zero")
					NatAsNumberZero
					)
				)
			)
		)
	))

