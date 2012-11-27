(js-repr NatAsNumber =
	(spec Nat)
)

(let NatAsNumberZero = (js-expr
	"0" :: NatAsNumber
	(spec Zero)
))

(let NatAsNumberSucc (x :: js-term NatAsNumber) = (js-expr
	"x + 1" :: NatAsNumber
	(= "x" x)
	(spec (Succ x))
))

(let NatAsNumberPlus (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x + y" :: NatAsNumber
	(= "x" x) (= "y" y)
	(spec (plus x y))
))

(let NatAsNumberTimes (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x * y" :: NatAsNumber
	(spec (times x y))
))

