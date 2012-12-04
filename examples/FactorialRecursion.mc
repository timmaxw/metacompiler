(let Factorial (a :: js-term NatAsNumber) :: (js-term NatAsNumber) = (js-global
	(type NatAsNumber)
	(NatAsNumberCase
		NatAsNumber
		a
		(NatAsNumberSucc NatAsNumberZero)
		(\ (b :: js-term NatAsNumber) -> NatAsNumberTimes a (Factorial b))
		)
	))

(emit "the_answer = x;" (= "x"
	(Factorial (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc (NatAsNumberSucc NatAsNumberZero))))))
	))

