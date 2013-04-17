(sl-code [[

	(data Either (l :: *) (r :: *) = (Left l) (Right r))

]])

(js-expr-type EitherAsPair
		(lSL :: sl-type "*") (lJS :: js-expr-type lSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL) =
	(spec (sl-type "Either l r" (type "l" = lSL) (type "r" = rSL)))
)

(let EitherAsPairLeft
		(lSL :: sl-type "*") (lJS :: js-expr-type lSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(xSL :: sl-term lSL) (xJS :: js-expr lJS xSL)
		= (js-expr
	(type (EitherAsPair lSL lJS rSL rJS))
	(spec (sl-term "Left l r . x" (type "l" = lSL) (type "r" = rSL) (term "x" = xSL)))
	(impl "['left', x]" (expr "x" = xJS))
))

(let EitherAsPairRight
		(lSL :: sl-type "*") (lJS :: js-expr-type lSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(xSL :: sl-term rSL) (xJS :: js-expr rJS xSL)
		= (js-expr
	(type (EitherAsPair lSL lJS rSL rJS))
	(spec (sl-term "Right l r . x" (type "l" = lSL) (type "r" = rSL) (term "x" = xSL)))
	(impl "['right', x]" (expr "x" = xJS))
))

(let EitherAsPairCase
		(lSL :: sl-type "*") (lJS :: js-expr-type lSL)
		(rSL :: sl-type "*") (rJS :: js-expr-type rSL)
		(subjectSL :: sl-term (sl-type "Either l r" (type "l" = lSL) (type "r" = rSL)))
		(subjectJS :: js-expr (EitherAsPair lSL lJS rSL rJS) subjectSL)
		(resTypeSL :: sl-type "*")
		(resTypeJS :: js-expr-type resTypeSL)
		(leftClauseSL :: fun (xSL :: sl-term lSL) -> sl-term resTypeSL)
		(leftClauseJS :: fun (xSL :: sl-term lSL) (xJS :: js-expr lJS xSL) -> js-expr resTypeJS (leftClauseSL xSL))
		(rightClauseSL :: fun (xSL :: sl-term rSL) -> sl-term resTypeSL)
		(rightClauseJS :: fun (xSL :: sl-term rSL) (xJS :: js-expr rJS xSL) -> js-expr resTypeJS (rightClauseSL xSL))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term "case subject of (Left l r . x) -> (lc . x) (Right l r . x) -> (rc . x)"
		(term "subject" = subjectSL)
		(type "l" = lSL)
		(type "r" = rSL)
		(term "lc" (xSL :: sl-term lSL) = leftClauseSL xSL)
		(term "rc" (xSL :: sl-term rSL) = rightClauseSL xSL)
	))
	(impl [[
		(function(s) {
			if (s[0] == 'left') {
				return lc(s[1]);
			} else {
				return rc(s[1]);
			}
		})(subject())
		]]
		(expr "subject" = subjectJS)
		(expr "lc" (xSL :: sl-term lSL | xJS :: js-expr lJS xSL) = leftClauseJS xSL xJS)
		(expr "rc" (xSL :: sl-term rSL | xJS :: js-expr rJS xSL) = rightClauseJS xSL xJS)
	)
))
