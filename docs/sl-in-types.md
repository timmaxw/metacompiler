# Making SL equivalents part of the type rather than part of the value

I am considering changing TL so that instead of the SL equivalent of a JS term being part of its value, the SL equivalent would be part of its type.

## Examples

The changes are easiest to show by example:

```
(js-expr-type NatAsNumber =
	(spec (sl-type "Nat"))
)

(let use NatAsNumberSucc (numSL :: sl-term Nat) (numJS :: js-expr NatAsNumber numSL) = (js-expr
	(type NatAsNumber)
	(spec (sl-term "Succ numSL"))
	(impl "num() + 1" (expr "num" = numJS))
))

(let use NatAsNumberPlus (aSL :: sl-term Nat) (bSL :: sl-term Nat) (aJS :: js-expr NatAsNumber aSL) (bJS :: js-expr NatAsNumber bSL) = (js-expr
	(type NatAsNumber)
	(spec (sl-term "plus aSL bSL"))
	(impl "a() + b()" (expr "a" = aJS) (expr "b" = bJS))
))

(let use NatAsNumberCase
		(resTypeSL :: sl-type)
		(resTypeJS :: js-expr-type resTypeSL)
		(subjectSL :: sl-term Nat)
		(subjectJS :: js-expr NatAsNumber subjectSL)
		(zeroClauseSL :: sl-term resTypeSL)
		(zeroClauseJS :: js-expr resTypeJS zeroClauseSL)
		(succClauseSL :: fun (x :: sl-term Nat) -> sl-term resTypeSL)
		(succClauseJS :: fun (x :: sl-term Nat) (y :: js-expr NatAsNumber x) -> js-expr resTypeJS (succClauseSL x))
		= (js-expr
	(type resTypeJS)
	(spec (sl-term
		"(case subjectSL of (Zero) -> (zc) (Succ a) -> (sc a))"
		(term "zc" = zeroClauseSL)
		(term "sc" (a :: sl-term Nat) = succClauseSL a)
		))
	(impl
		[[
			(function(s) {
				if (s == 0) { return zc(); }
				else { return sc(s); }
			})(subj())
		]]
		(expr "subj" = subjectJS)
		(expr "zc" = zeroClauseJS)
		(expr "sc" (innerSL :: sl-term Nat | innerJS :: js-term NatAsNumber innerSL) = succClauseJS innerSL innerJS)
		)
))

(js-expr-type FunctionType
		(argTypeSL :: sl-type)
		(argTypeJS :: js-expr-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-expr-type retTypeSL) =
	(spec (sl-type "fun a -> r" (type "a" = argTypeSL) (type "r" = retTypeSL)))
)

(let FunctionLambda
		(argTypeSL :: sl-type)
		(argTypeJS :: js-expr-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-expr-type retTypeSL)
		(bodySL :: fun (argSL :: sl-term argTypeSL) -> sl-term argTypeJS)
		(bodyJS :: fun (argSL :: sl-term argTypeSL) (argJS :: js-expr argTypeJS argSL) -> js-expr retTypeJS (bodySL argSL))
		= (js-expr
	(type (FunctionType argTypeSL argTypeJS retTypeSL retTypeJS))
	(spec (sl-term "\ x -> b x" (term "b" (x :: sl-term argTypeSL) = bodySL x)))
	(impl
		"function(x) { return body(x); }"
		(expr "body" (argSL :: sl-term argTypeSL | argJS :: js-term argTypeJS xSL) = bodyJS xSL xJS)
		)
))

(let FunctionApply
		(argTypeSL :: sl-type)
		(argTypeJS :: js-expr-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-expr-type retTypeSL)
		(funSL :: sl-term (fun argTypeSL -> retTypeSL))
		(funJS :: js-expr (FunctionType argTypeSL argTypeJS retTypeSL retTypeJS) funSL)
		(argSL :: sl-term argTypeSL)
		(argJS :: js-expr argTypeJS argSL)
		= (js-expr
	(type retTypeJS)
	(spec (sl-term "f a" (term "f" = funSL) (term "a" = argSL)))
	(impl ("f(a)" (expr "f" = funJS) (expr "a" = argJS)))
))
```

## Discussion

There are a couple of benefits of this system over the existing one:

* It puts all the public information about a meta-object into that meta-object's type. The SL equivalent of something should be considered a part of that thing's public interface.

* Previously, there was a distinction between `(js-term ...)` and `(js-sl-term ...)`. I have always suspected that this distinction indicated something wrong with the theory. This gets rid of the distinction.

* It allows for some neat constructs. For example, suppose I want to choose between two equivalent algorithms at runtime based on the input. I could write something like this:

    ```
	(let ChooseAtRuntime
			(typeSL :: sl-type)
			(typeJS :: js-expr-type typeSL)
			(criterionSL :: sl-term Bool)
			(criterionJS :: js-expr BoolAsJSBool criterionSL)
			(valueSL :: sl-term typeSL)
			(valueJSIfCriterionTrue :: js-expr typeJS valueSL)
			(valueJSIfCriterionFalse :: js-expr typeJS valueSL)
			= (js-expr
		(type typeJS)
		(spec valueSL)
		(impl 
			[[
				(function() {
					if (c()) {
						return t();
					} else {
						return f();
					}
				})()
			]]
			(expr "c" = criterionJS)
			(expr "t" = valueJSIfCriterionTrue)
			(expr "f" = valueJSIfCriterionFalse)
			)
	))
	```

	This will select one of the two algorithms at runtime based on the supplied criterion. It uses the type system to require that the two algorithms be equivalent. While this ability is certainly not necessary for `metacompiler` to be useful, the fact that this is possible suggests that putting SL equivalents into the type system is a "natural" thing to do.

One disadvantage of the proposed changes is that they make things much more verbose. Perhaps the amount of verbiage could be reduced by introducing polymorphism and type inference.


