(js-repr NatAsNumber =
	(spec Nat)
)

(let NatAsNumberZero = (js-expr
	"0"
	(type NatAsNumber)
	(spec Zero)
))

(let NatAsNumberSucc (x :: js-term NatAsNumber) = (js-expr
	"x + 1"
	(= "x" x)
	(type NatAsNumber)
	(spec (Succ x))
))

(let NatAsNumberPlus (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x + y"
	(= "x" x) (= "y" y)
	(type NatAsNumber)
	(spec (plus x y))
))

(let NatAsNumberTimes (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x * y"
	(= "x" x) (= "y" y)
	(type NatAsNumber)
	(spec (times x y))
))

