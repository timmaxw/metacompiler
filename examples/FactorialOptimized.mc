(sl-code [[

	(let factorial . (x :: Nat) :: Nat =
		(case x of
			(Zero .) -> (Succ . Zero)
			(Succ . y) -> (times . x (factorial . y))
		)
	)

]])

(let Factorial (xSL :: sl-term (sl-type "Nat")) (xJS :: js-expr NatAsNumber xSL) = (js-expr
	(type NatAsNumber)
	(spec (sl-term "factorial . x" (term "x" = xSL)))
	(impl [[
		(function(a) {
			var b = 1;
			while (a > 0) {
				b *= a;
				a--;
			}
			return b;
		})(x())
		]]
		(expr "x" = xJS)
	)
))

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

