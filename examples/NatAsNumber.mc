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

(let NatAsNumberCase
        (res :: js-type)
        (subject :: js-term NatAsNumber)
        (zeroClause :: js-term res) (succClause :: fun (a :: js-term NatAsNumber) -> js-term res)
        = (js-expr
    [[
        (function(s) {
            if (s == 0) { return zc; }
            else { return sc; }
        })(subj)
    ]]
    (= "zc" zeroClause)
    (= "sc" (succClause (js-expr "s-1" (type NatAsNumber))))
    (= "subj" subject)
    (type res)
    (spec (case subject of (Zero) -> (zeroClause) (Succ a) -> (succClause a)))
))
