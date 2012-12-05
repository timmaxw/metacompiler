# Making SL equivalents part of the type rather than part of the value

I am considering changing TL so that instead of the SL equivalent of a JS term being part of its value, the SL equivalent would be part of its type. This is still very much a work in progress; there are some things that I'm unsure how to express under the proposed system. Specifically, it's not clear how to introduce new variables into scope in a `(js-expr ...)`, such as when implementing `case` or functions.

## Summary of proposed changes

Two new meta-types are introduced. `sl-type` is the meta-type of SL types. `sl-term <type>` is the meta-type of SL terms having type `<type>`.

The `js-term` meta-type is removed and the `js-sl-term` meta-type is renamed to `js-term`. The `js-type` meta-type gets a parameter of meta-type `sl-type`, which is its SL equivalent. The `js-term` meta-type gets an additional parameter of meta-type `sl-term x`, where `x` is the SL equivalent of the first parameter to `js-term`.

## Examples

Note that I have put `?` where I'm not sure what could plausibly go there.

```
(let use NatAsNumberSucc (numSL :: sl-term Nat) (numJS :: js-term NatAsNumber numSL) = (js-expr
	(type NatAsNumber)
	(spec (Succ numSL)
	(= "num" numJS)
	"num + 1"
))

(let use NatAsNumberPlus (aSL :: sl-term Nat) (bSL :: sl-term Nat) (aJS :: js-term NatAsNumber aSL) (bJS :: js-term NatAsNumber bSL) = (js-expr
	(type NatAsNumber)
	(spec (plus aSL bSL))
	(= "a" aJS) (= "b" bJS)
	"a + b"
))

(let use NatAsNumberCase
		(resTypeSL :: sl-type)
		(resTypeJS :: js-type resTypeSL)
		(subjectSL :: sl-term Nat)
		(subjectJS :: js-term NatAsNumber subjectSL)
		(zeroClauseSL :: sl-term resTypeSL)
		(zeroClauseJS :: js-term resTypeJS zeroClauseSL)
		(succClauseSL :: fun (x :: sl-term Nat) -> sl-term resTypeSL)
		(succClauseJS :: fun (x :: sl-term Nat) (y :: js-term NatAsNumber x) -> js-term resTypeJS (succClauseSL x))
		= (js-expr
	(type resTypeJS)
	(spec (case subjectSL of (Zero) -> (zeroClauseSL) (Succ a) -> (succClauseSL a)))
	(= "subj" subjectJS)
	(= "zc" zeroClauseJS)
	(= "sc" (succClauseJS ? ?))
	[[
		(function(s) {
			if (s == 0) { return zc; }
			else { return sc; }
		})(subj)
	]]
))

(js-repr FunctionType
		(argTypeSL :: sl-type)
		(argTypeJS :: js-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-type retSL) =
	(spec (fun argTypeSL -> retTypeSL))
)

(let FunctionLambda
		(argTypeSL :: sl-type)
		(argTypeJS :: js-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-type retTypeSL)
		(bodySL :: fun (argSL :: sl-term argTypeSL) -> sl-term argTypeJS)
		(bodyJS :: fun (argSL :: sl-term argTypeSL) (argJS :: js-term argTypeJS argSL) -> sl-term retTypeJS (bodySL argSL))
		= (js-expr
	(type (FunctionType argTypeSL argTypeJS retTypeSL retTypeJS))
	(spec (\ x -> bodySL x))
	(= "body" (bodyJS ? ?))
	"function(x) { return body; }"
))

(let FunctionApply
		(argTypeSL :: sl-type)
		(argTypeJS :: js-type argTypeSL)
		(retTypeSL :: sl-type)
		(retTypeJS :: js-type retTypeSL)
		(funSL :: sl-term (fun argTypeSL -> retTypeSL))
		(funJS :: js-term (FunctionType argTypeSL argTypeJS retTypeSL retTypeJS) funSL)
		(argSL :: sl-term argTypeSL)
		(argJS :: js-term argTypeJS argSL)
		= (js-expr
	(type retTypeJS)
	(spec (funSL argSL))
	(= "fun" fun)
	(= "arg" arg)
	"fun(arg)"
))
```

## Discussion

There are a couple of benefits of this system over the existing one:

* It puts all the public information about a meta-object into that meta-object's type. The SL equivalent of something should be considered a part of that thing's public interface.

* Previously, there was a distinction between `(js-term ...)` and `(js-sl-term ...)`. I have always suspected that this distinction indicated something wrong with the theory. This gets rid of the distinction.

* It allows for some neat constructs. For example, suppose I want to choose between two equivalent algorithms at runtime based on the input. I could write something like this:

    ```
	let (ChooseAtRuntime
			(typeSL :: sl-type)
			(typeJS :: js-type typeSL)
			(criterionSL :: sl-term Bool)
			(criterionJS :: js-term BoolAsJSBool criterionSL)
			(valueSL :: sl-term typeSL)
			(valueJSIfCriterionTrue :: js-term typeJS valueSL)
			(valueJSIfCriterionFalse :: js-term typeJS valueSL)
			= (js-repr
		(type typeJS)
		(spec valueSL)
		(= "criterion" criterionJS)
		(= "valueIfCriterionTrue" valueJSIfCriterionTrue)
		(= "valueIfCriterionFalse" valueJSIfCriterionFalse)
		[[
			(function() {
				if (criterion) {
					return valueIfCriterionTrue;
				} else {
					return valueIfCriterionFalse;
				}
			})()
		]]
	))
	```

	This will select one of the two algorithms at runtime based on the supplied criterion. It uses the type system to require that the two algorithms be equivalent. While this ability is certainly not necessary for `metacompiler` to be useful, the fact that this is possible suggests that putting SL equivalents into the type system is a "natural" thing to do.

One disadvantage of the proposed changes is that they make things much more verbose. Perhaps the amount of verbiage could be reduced by introducing polymorphism and type inference.

The other disadvantage is, obviously, that it's not clear how to express the idea of bringing a variable into scope with the new system.

