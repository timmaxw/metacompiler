(sl-code [[

	(data Maybe (a :: *) = (Nothing) (Just a))

]])

(js-expr-type MaybeAsNull
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL) =
	(spec (sl-type "Maybe a" (type "a" = aSL)))
)

(let MaybeAsNullNothing
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		= (js-expr
	(type (MaybeAsNull aSL aJS))
	(spec (sl-term "Nothing a ." (type "a" = aSL)))
	(impl "null")
))

(let MaybeAsNullJust
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term aSL) (xJS :: js-expr aJS xSL)
		= (js-expr
	(type (MaybeAsNull aSL aJS))
	(spec (sl-term "Just a . x" (type "a" = aSL) (term "x" = xSL)))
	(impl "x" (expr "x" = xJS))
))

(let MaybeAsNullCase
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(subjectSL :: sl-term (sl-type "Maybe a" (type "a" = aSL)))
		(subjectJS :: js-expr (MaybeAsNull aSL aJS) subjectSL)
		(resTypeSL :: sl-type "*")
		(resTypeJS :: js-expr-type resTypeSL)
		(nothingClauseSL :: sl-term resTypeSL)
		(nothingClauseJS :: js-expr resTypeJS nothingClauseSL)
		(justClauseSL :: fun (xSL :: sl-term aSL) -> sl-term resTypeSL)
		(justClauseJS :: fun (xSL :: sl-term aSL) (xJS :: js-expr aJS xSL) -> js-expr resTypeJS (justClauseSL xSL))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term "case subject of (Nothing a .) -> (nc .) (Just a . x) -> (jc . x)"
		(term "subject" = subjectSL)
		(type "a" = aSL)
		(term "nc" = nothingClauseSL)
		(term "jc" (xSL :: sl-term aSL) = justClauseSL xSL)
	))
	(impl [[
		(function(s) {
			if (s === null) {
				return nc();
			} else {
				return jc(s);
			}
		})(subject())
		]]
		(expr "subject" = subjectJS)
		(expr "nc" = nothingClauseJS)
		(expr "jc" (xSL :: sl-term aSL | xJS :: js-expr aJS xSL) = justClauseJS xSL xJS)
	)
))

