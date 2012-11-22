# Recursion

TL will support a new construct called `(js-global <term>)`. Loosely speaking, `(js-global <term>)` represents a reference to a global variable that is defined to be equivalent to `<term>`.

## Details of `js-global`

In the abstract syntax tree data type, it looks like:

    MOJSGlobal {
        tagOfMetaObject :: a,
        uniqueIdentifierOfMetaObject :: JSGlobalUnique,
        referentOfMetaObject :: MetaObject a
    }

`uniqueIdentifierOfMetaObject` is generated from the line-and-column position of the `js-global` term.

`<term>` must have meta-type `js-expr ...`. `(js-global <term>)` has the same meta-type and the same SL equivalent as `<term>`.

## JavaScript equivalent of `js-global`

Here's how the JavaScript equivalent of `MOJSGlobal` works: As `metacompiler` generates JavaScript equivalents for terms, it maintains a map with the following type:

    Map (JSGlobalUnique, Map String JSType) (String, [String])

When it encounters a `MOJSGlobal`, it looks through `referentOfMetaObject` and finds every variable that is currently in scope that `referentOfMetaObject` refers to. It classifies the variables into three types:

* Variables with meta-type `js-type`. These will be handled at compile time.

* Variables with meta-type `js-term ...`. These will be handled at runtime.

* Other variables. These result in a compile-time error.

It collects all the `js-type` variables into a `Map String JSType`, then uses that along with `uniqueIdentifierOfMetaObject` as a key in the map. If an entry is already present in the map, it uses that entry. If not, it creates a new one as follows:

First, it assigns an ordering to the `js-term ...` variables, and picks a unique identifier `<global-name>`. Then it inserts `("<global-name>", ["<var1>", "<var2>", ...])` into the map under the missing key. Then it computes the JavaScript equivalent of `referentOfMetaObject`, with that entry in the map. Finally, at the top level of the output JavaScript file, it emits:

    function <global-name> (<var1>, <var2>, ...) {
    	return <javascript-equivalent>;
    }

Now the entry must be present in the map, either because it was put there earlier or because it was just created. The JavaScript equivalent of the `js-global` terms is now:

    <global-name>(<var1>, <var2>, ...)

## Examples

### Trivial example

TL:

    (emit "the_answer = x;" (set "x"
    	(js-global NatAsNumberSucc (NatAsNumberSucc (NatAsNumberZero))
    	)

JavaScript:

    function _1() {
        return 1 + (1 + 0);
    }

	the_answer = _1();

### Slightly more complicated

TL:

	(emit "the_answer = x;" (set "x"
		(js-global (js-global (js-global NatAsNumberSucc (NatAsNumberSucc (NatAsNumberZero))))
		)

JavaScript:

	function _1() {
		return _2();
	}

	function _2() {
		return _3();
	}

	function _3() {
		return 1 + (1 + 0);
	}

	the_answer = _1();

### Actual recursion

SL:

	(let factorial (a :: Nat) :: Nat =
		(case a of
			(Zero) -> (Succ Zero)
			(Succ b) -> (prod a (factorial b))
		)
	)

TL:

	(let Factorial (a :: js-expr NatAsNumber) :: js-expr NatAsNumber = (js-global
		NatAsNumberCase
			NatAsNumber
			a
			(NatAsNumberSucc NatAsNumberZero)
			(fun (b :: NatAsNumber) -> NatAsNumberProd a (Factorial b))
		))

	(emit "the_answer = x;" (set "x"
		(Factorial (Succ (Succ (Succ (Succ (Succ Zero))))))
		)

JavaScript:

	function _1(a) {
		return (function(s) {
			if (s == 0) { return 1 + 0; }
			else { return a * _1(s-1); }
		})(a);
	}

	the_answer = _1(1 + (1 + (1 + (1 + (1 + 0)))))

## Extensions

The code produced by this process is sometimes unnecessarily verbose. If it's trivial to prove that `referentOfMetaObject` always terminates, and if there are no arguments, then the `function() { return ...; }` can be omitted.

