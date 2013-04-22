(sl-code [[

	(data List (a :: *) = (Nil) (Cons a (List a)))

	(let length (a :: *) . (l :: List a) :: Nat =
		(case l of
			(Nil a .) -> (Zero .)
			(Cons a . x xs) -> (Succ . (length a . xs))
		)
	)

	(let concat (a :: *) . (left :: List a) (right :: List a) :: (List a) =
		(case left of
			(Nil a .) -> (right)
			(Cons a . x xs) -> (Cons a . x (concat a . xs right))
		)
	)

	(let map (a :: *) (b :: *) . (f :: fun a -> b) (l :: List a) :: (List b) =
		case l of
		    (Nil a .) -> (Nil b .)
		    (Cons a . x xs) -> (Cons b . (f . x) (map a b . f xs))
	)
]])

(js-expr-type ListAsArray (aSL :: sl-type "*") (aJS :: js-expr-type aSL) =
	(spec (sl-type "List a" (type "a" = aSL)))
)

(let ListAsArrayNil
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "Nil a ." (type "a" = aSL)))
	(impl "[]")
))

(let ListAsArrayCons
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term aSL) (xJS :: js-expr aJS xSL)
		(xsSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xsJS :: js-expr (ListAsArray aSL aJS) xsSL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "Cons a . x xs" (type "a" = aSL) (term "x" = xSL) (term "xs" = xsSL)))
	(impl "[x()].concat(xs())" (expr "x" = xJS) (expr "xs" = xsJS))
))

(let ListAsArrayCase
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(subjectSL :: sl-term (sl-type "List a" (type "a" = aSL)))
		(subjectJS :: js-expr (ListAsArray aSL aJS) subjectSL)
		(resTypeSL :: sl-type "*")
		(resTypeJS :: js-expr-type resTypeSL)
		(nilClauseSL :: sl-term resTypeSL)
		(nilClauseJS :: js-expr resTypeJS nilClauseSL)
		(consClauseSL :: fun
			(xSL :: sl-term aSL)
			(xsSL :: sl-term (sl-type "List a" (type "a" = aSL)))
			-> sl-term resTypeSL)
		(consClauseJS :: fun
			(xSL :: sl-term aSL)
			(xJS :: js-expr aJS xSL)
			(xsSL :: sl-term (sl-type "List a" (type "a" = aSL)))
			(xsJS :: js-expr (ListAsArray aSL aJS) xsSL)
			-> js-expr resTypeJS (consClauseSL xSL xsSL))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term "case subject of (Nil a .) -> (nc .) (Cons a . x xs) -> (cc . x xs)"
		(term "subject" = subjectSL)
		(type "a" = aSL)
		(term "nc" = nilClauseSL)
		(term "cc" (xSL :: sl-term aSL) (xsSL :: sl-term (sl-type "List a" (type "a" = aSL))) = consClauseSL xSL xsSL)
	))
	(impl [[
		(function(s) {
			if (s.length == 0) {
				return nc;
			} else {
				return cc(s[0], s.slice(1));
			}
		})(subject)
		]]
		(expr "subject" = subjectJS)
		(expr "nc" = nilClauseJS)
		(expr "cc"
			(xSL :: sl-term aSL | xJS :: js-expr aJS xSL)
			(xsSL :: sl-term (sl-type "List a" (type "a" = aSL)) | xsJS :: js-expr (ListAsArray aSL aJS) xsSL)
			= consClauseJS xSL xJS xsSL xsJS)
	)
))

(let ListAsArrayLength
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xJS :: js-expr (ListAsArray aSL aJS) xSL)
		= (js-expr
	(type NatAsNumber)
	(spec (sl-term "length a . x" (type "a" = aSL) (term "x" = xSL)))
	(impl "x().length" (expr "x" = xJS))
))

(let ListAsArrayConcat
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(xSL :: sl-term (sl-type "List a" (type "a" = aSL))) (xJS :: js-expr (ListAsArray aSL aJS) xSL)
		(ySL :: sl-term (sl-type "List a" (type "a" = aSL))) (yJS :: js-expr (ListAsArray aSL aJS) ySL)
		= (js-expr
	(type (ListAsArray aSL aJS))
	(spec (sl-term "concat a . x y" (type "a" = aSL) (term "x" = xSL) (term "y" = ySL)))
	(impl "x().concat(y())" (expr "x" = xJS) (expr "y" = yJS))
))

(let ListAsArrayMap
		(aSL :: sl-type "*") (aJS :: js-expr-type aSL)
		(bSL :: sl-type "*") (bJS :: js-expr-type bSL)
		(fSL :: sl-term (sl-type "fun a -> b" (type "a" = aSL) (type "b" = bSL)))
		(fJS :: js-expr (FunctionType aSL aJS bSL bJS) fSL)
		(lSL :: sl-term (sl-type "List a" (type "a" = aSL)))
		(lJS :: js-expr (ListAsArray aSL aJS) lSL)
		= (js-expr
	(type (ListAsArray bSL bJS))
	(spec (sl-term "map a b . f l" (type "a" = aSL) (type "b" = bSL) (term "f" = fSL) (term "l" = lSL)))
	(impl [[
		(function (f, l) {
			var l2 = [];
			for (var x in l) {
				l2.push(f(x));
			}
			return l2;
		})(fun(), list())
		]]
		(expr "fun" = fJS)
		(expr "list" = lJS)
	)
))
