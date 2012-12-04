(js-repr NatAsNumber =
	(spec Nat)
)

(let NatAsNumberZero = (js-expr
	(type NatAsNumber)
	(spec Zero)
	"0"
))

(let NatAsNumberSucc (x :: js-term NatAsNumber) = (js-expr
	(type NatAsNumber)
	(spec (Succ x))
	(= "x" x)
	"x + 1"
))

(let NatAsNumberPlus (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	(type NatAsNumber)
	(spec (plus x y))
	(= "x" x) (= "y" y)
	"x + y"
))

(let NatAsNumberTimes (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	(type NatAsNumber)
	(spec (times x y))
	(= "x" x) (= "y" y)
	"x * y"
))

(let NatAsNumberCase
		(res :: js-type)
		(subject :: js-term NatAsNumber)
		(zeroClause :: js-term res) (succClause :: fun (a :: js-term NatAsNumber) -> js-term res)
		= (js-expr
	(type res)
	(spec (case subject of (Zero) -> (zeroClause) (Succ a) -> (succClause a)))
	(= "zc" zeroClause)
	(= "sc" (succClause (js-expr (type NatAsNumber) "s-1")))
	(= "subj" subject)
	[[
		(function(s) {
			if (s == 0) { return zc; }
			else { return sc; }
		})(subj)
    ]]
))
