# Examples

**Note: Because the translation language has been changing rapidly recently, this document is somewhat out-of-date.**

This is a collection of examples of SL and translation-language code.

## A simple example: translating Peano numbers into Javascript numbers

Suppose that we have the following SL definitions:

```
(data Nat = (Zero) (Succ Nat))

(let plus (a :: Nat) (b :: Nat) :: Nat = case a of
	Zero -> b
	(Succ a') -> (Succ (plus a' b))
)

(let times (a :: Nat) (b :: Nat) :: Nat = case a of
	Zero -> Zero
	(Succ a') -> (plus b (times a' b))
```

Ignoring integer overflow, a natural translation is to express `Nat` as a Javascript number and `plus` as Javascript addition. We can define those translations as follows:

```
(js-repr NatAsNumber =
	(spec Nat)
)

(let use NatAsNumberZero = (js-expr
	"0" :: NatAsNumber
	(spec Zero)
))

(let use NatAsNumberSucc (x :: js-term NatAsNumber) = (js-expr
	"x + 1" :: NatAsNumber
	(= "x" x)
	(spec (Succ x))
))

(let use NatAsNumberPlus (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x + y" :: NatAsNumber
	(= "x" x) (= "y" y)
	(spec (plus x y))
))

(let use NatAsNumberTimes (x :: js-term NatAsNumber) (y :: js-term NatAsNumber) = (js-expr
	"x * y" :: NatAsNumber
	(spec (times x y))
))

(emit "two = x" :
	(= "x" (infer
		(goal (plus (Succ Zero) (Succ Zero)))
		(type NatAsNumber)
	))
)
```

When `metacompiler` encounters the `infer`, it will compute the following translation-language term:

```
(NatAsNumberPlus (NatAsNumberSucc (NatAsNumberZero)) (NatAsNumberSucc (NatAsNumberZero)))
```

and then emit the following Javascript, or something similar:

```
two = (((0) + 1) + ((0) + 1))
```

## Telling `metacompiler` how to handle case-expressions

In our definition of `NatAsNumber` above, we haven't told the compiler how to translate constructs of the form `case x of ...` where `x` is a `Nat`. Here's how to do that:

```
(let use NatAsNumberCase
		(res :: js-type)
		(subject :: js-term NatAsNumber)
		(zeroClause :: js-term res) (succClause :: fun (js-term NatAsNumber) -> js-term res)
		= (js-expr
	[[
		(function(s) {
			if (s == 0) { return zc; }
			else { return sc; }
		})(subj)
	]]
	(= "zc" zeroClause)
	(= "sc" (succClause "s-1"))
	(= "subj" subject)
	(type res)
	(spec (case subject of (Zero) -> (zeroClause) (Succ a) -> (succClause a)))
	)
))
```

(The `[[` and `]]` are an alternative syntax for string literals in S-expressions; see `overview.md` for details.)

## Translating `Either` to Javascript

SL definition:

```
(data Either (l :: *) (r :: *) = (Left l) (Right r))
```

Translations:

```
(js-repr EitherAsPair (l :: js-type) (r :: js-type) =
	(spec (Either l r))
)

(let use EitherAsPairLeft (l :: js-type) (r :: js-type) (x :: js-term l) = (js-expr
	"['left', x]"
	(= "x" x)
	(type (EitherAsPair l r))
	(spec (Left l r x))
))

(let use EitherAsPairRight (l :: js-type) (r :: js-type) (x :: js-term r) = (js-expr
	"['right', x]"
	(= "x" x)
	(type (EitherAsPair l r))
	(spec (Right l r x))
))

(let use EitherAsPairCase
		(l :: js-type) (r :: js-type) (res :: js-type)
		(subject :: js-term (EitherAsPair l r))
		(leftClause :: fun (js-term l) -> js-term res)
		(rightClause :: fun (js-term r) -> js-term res)
		= (js-expr
	[[
		(function(s) {
			if (s[0] == 'left') { return leftClause; }
			else { return rightClause; }
		})(subject)
	]]
	(= "subject" subject)
	(= "leftClause"  (leftClause (js-expr "s[1]" (type l))))
	(= "rightClause" (rightClause (js-expr "s[1]" (type r))))
	(type res)
	(spec (case subject of
		(Left x) -> (leftClause x)
		(Right x) -> (rightClause x)
	))
))
```

## Translating functions to Javascript

`metacompiler` does not have built-in knowledge of Javascript functions. The user must specify how to translate the SL concept of a function into Javascript. Here is a typical such translation:

```
(js-repr FunctionType
		(a :: js-type) (r :: js-type) =
	(spec (fun a -> r))
)

(let use FunctionLambda
		(a :: js-type) (r :: js-type)
		(body :: fun (js-term a) -> js-term r)
		= (js-expr
	"function(x) { return body; }"
	(= "body" (body (js-expr "x" (type a))))
	(type (FunctionType a r))
	(spec (\ x -> body x))
))

(let use FunctionApply
		(a :: js-type) (r :: js-type)
		(fun :: js-term (FunctionType a r)) (arg :: js-term a)
		= (js-expr
	"fun(arg)"
	(= "fun" fun)
	(= "arg" arg)
	(type r)
	(spec (fun arg))
))
```

## Multiple-parameter functions

Suppose that we want `metacompiler` to turn SL functions that take multiple parameters (curried, of course) into Javascript functions that take multiple parameters. Here's a way to do that:

```
(js-repr Function2Type
		(a1 :: js-type) (a2 :: js-type) (r :: js-type) =
	(spec (fun a1 a2 -> r))
)

(let use Function2Lambda
		(a1 :: js-type) (a2 :: js-type) (r :: js-type)
		(body :: fun (js-term a1) (js-term a2) -> js-term r)
		= (js-expr
	"function(x1, x2) { return body; }"
	(= "body" (body (js-expr "x1" (type a1)) (js-expr "x2" (type a2))))
	(type (Function2Type a1 a2 r))
	(spec (\ x1 x2 -> body x1 x2))
))

(let use Function2Apply
		(a1 :: js-type) (a2 :: js-type) (r :: js-type)
		(fun :: js-term (Function2Type a1 a2 r)) (arg1 :: js-term a1) (arg2 :: js-term a2)
		= (js-expr
	"fun(arg1, arg2)"
	(= "fun" fun)
	(= "arg1" arg1)
	(= "arg2" arg2)
	(type r)
	(spec (fun arg1 arg2))
))
```

## Lazy values

Just like for functions, `metacompiler` must be taught how to represent lazily computed values:

```
(js-repr LazyType (a :: js-type) =
	(spec (lazy a))
)

(let use LazyWrap (a :: js-type) (unwrapped :: js-term a) = (js-expr
	"function() { return unwrapped; }"
	(= "unwrapped" unwrapped)
	(type (LazyType a))
	(spec (wrap unwrapped))
))

(let use LazyUnwrap (a :: js-type) (wrapped :: js-term (LazyType a)) = (js-expr
	"wrapped()"
	(= "wrapped" wrapped)
	(type a)
	(spec (unwrap wrapped))
))
```

## Representing `Maybe` using `null`

A common convention in Javascript is to represent an optional value by using `null` in the case where the value is absent. The corresponding convention in Haskell is to use the type `Maybe a` to represent an optional value. Here's an example of how the latter convention can be translated into the former convention.

SL file:

```
(data Maybe (a :: *) = (Nothing) (Just a))
```

Translation language file:

```
(js-repr MaybeAsNull (a :: js-type) =
	(spec (Maybe a))
)

(let MaybeAsNullNothing (a :: js-type) = (js-expr
	"null"
	(type (MaybeAsNull a))
	(spec (Nothing a))
))

(let MaybeAsNullJust (a :: js-type) (val :: js-term a) = (js-expr
	"x"
	(= "x" val)
	(type (MaybeAsNull a))
	(spec (Just a val))
))

(let MaybeAsNullCase
		(a :: js-type) (res :: js-type)
		(subject :: js-term (MaybeAsNull a))
		(nothingClause :: js-term res)
		(justClause :: fun (js-term a) -> js-term res)
		= (js-expr
	[[
		(function(s) {
			if (s == null) { return nc; }
			else { return jc; }
		})(subj)
	]]
	(= "subj" subject)
	(= "nc" nothingClause)
	(= "jc" (justClause (js-expr "s" (type a))))
	(type res)
	(spec (case subject of (Nothing) -> (nothingClause) (Just x) -> (justClause x)))
))
```

Note that we do not say `(let use ...` or `(use ...`. This is because the `MaybeAsNull` translation is not suitable in all situations. In particular, if a type maps some SL value to Javascript `null`, then that type cannot be used as the parameter to `MaybeAsNull`. The user must manually check each type and tell `metacompiler` that it is safe to use. For example, `NatAsNumber` is safe to use, and we can tell the compiler that like so:

```
(use (MaybeAsNullNothing NatAsNumber))
(use (MaybeAsNullJust NatAsNumber))
(use (MaybeAsNullCase NatAsNumber))
```

## Optimizing `factorial`

Suppose that we have the following SL implementation of factorial:

```
(let factorial (a :: Nat) :: Nat = case a of
	(Zero) -> (Succ Zero)
	(Succ a') -> (times a (factorial a'))
)
```

`metacompiler` will by default generate a recursive implementation of this, but suppose that we want an iterative implementation for performance reasons. We can get it as follows:

```
(let use Factorial = (js-expr
	[[
		function(a) {
			var x = 1;
			while (a > 0) {
				x *= a;
				a--;
			}
			return x;
		}
	]]
	(type (FunctionType NatAsNumber NatAsNumber))
	(spec factorial)
))
```

## Another simple example: lists and `map`

SL:

```
(data List (a :: *) = (Nil) (Cons a (List a)))

(let map (f :: fun a -> b) (l :: List a) :: List b =
	case l of
		(Nil) -> (Nil)
		(Cons x xs) -> (Cons (f x) (map f xs))
)
```

Translations:

```
(js-repr ListAsArray (a :: js-type) =
	(spec (List a))
)

(let use ListAsArrayNil (a :: js-type) = (js-expr
	"[]"
	(type (ListAsArray a))
	(spec Nil)
))

(let use ListAsArrayCons
		(a :: js-type) (x :: js-term a) (xs :: js-term (ListAsArray a))
		= (js-expr
	(type (ListAsArray a))
	(spec (Cons x xs))
	(impl "[x].concat(xs)" : (set "x" x) (set "xs" xs))
))

(let use ListAsArrayCase
		(a :: js-type) (res :: js-type)
		(subject :: js-term (ListAsArray a))
		(nilClause :: js-term res)
		(consClause :: fun (js-term a) (js-term (ListAsArray a)) -> js-term res)
		= (js-expr
	[[
		(function(s) {
			if (s.length == 0) { return nc; }
			else { return cc; }
		})(subj)
	]]
	(= "subj" subject)
	(= "nc" nilClause)
	(= "cc" (consClause "s[0]" "s.slice(1, s.length)"))
	(type res)
	(spec (case subject of (Nil) -> (nilClause) (Cons x xs) -> (ConsClause x xs)))
))

(let use ListAsArrayMap
		(a :: js-type) (b :: js-type)
		= (js-expr
	[[
		function(f, l) {
			var l2 = [];
			for (var x in l) {
				l2.push(f(x));
			}
			return l2;
		}
	]]
	(type (Function2Type (FunctionType a b) (ListAsArray a) (ListAsArray b)))
	(spec map)
))
```

## Advanced example: optimizing lists of consecutive integers

In the Python programming language (and many others), there is the concept of an "iterable". The consumer of an iterable object can repeatedly request values from it until they reach the end. Python lists are iterables, but so are many other things. For example, iterating over a file will produce each line of the file. Python's `reversed()` function will return an iterable object that lazily returns elements from the end of a list. Python's `xrange()` function will return an object that acts like a list of evenly spaced integers, but internally is represented by only three values (start, stop, and step).

Haskell programmers usually use lists for everything that Python programmers use iterables for. This is possible because Haskell lists are lazy, so in practice creating a list of all the numbers from one to a billion and then iterating over it takes a constant amount of space. But SL discourages laziness, so this is impractical. We can get a similar effect with `metacompiler` by special-casing lists of consecutive integers in the translation file. Here's how it's done.

First, we need a SL function to generate ranges of integers:

```
(let allNatsLessThan (a :: Nat) :: (List Nat) = case a of
	(Zero) -> (Nil)
	(Succ a') -> (Cons a' (allNatsLessThan a'))
)
```

(This will generate lists that count down; that's simpler to implement than the reverse.)

`metacompiler` will infer a perfectly valid implementation of this that produces lists of numbers. But the point was to make a better implementation. Here it is:

```
; A `CountDownList` represents a list of integers decreasing to 0 as a single
; number, namely one more than the first integer in the list.
(js-repr CountDownList =
	(spec (List Nat))
)

(let CountDownListNil = (js-expr
	"0"
	(spec Nil)
	(type CountDownList)
))

(let CountDownListAllNatsLessThan (a :: js-term NatAsNumber) = (js-expr
	"a"
	(= "a" a)
	(spec (allNatsLessThan a))
	(type CountDownList)
))

(let CountDownListCase
		(res :: js-type)
		(subject :: js-term CountDownList)
		(nilClause :: js-term res)
		(consClause :: fun (js-term NatAsNumber) (js-term CountDownList) -> js-term res)
		= (js-expr
	[[
		(function(n) {
			if (n == 0) { return nc; }
			else { return cc; }
		}(subj)
	]]
	(= "nc" nilClause)
	(= "cc" (consClause "n" "n-1"))
	(= "subj" subject)
	(spec (case subject of (Nil) -> (nilClause) (Cons x xs) -> (consClause x xs)))
	(type res)
))
```

If an SL term creates a list using `allNatsLessThan` and then recursively decomposes it using `case ... of ...`, but does nothing else with it, then `metacompiler` can use `CountDownList` in place of `ListAsArray NatAsNumber`, and the performance will be better. (Of course, the performance improvement is probably insignificant except for very large lists.)

Note that even though the SL equivalent of `CountDownList` is `List Nat`, not all values of type `List Nat` can be represented as a `CountDownList`. But that's OK.

We can also teach `metacompiler` how to convert `CountDownList`s into `ListAsArray NatAsNumber`s:

```
(let ConvertCountDownList (in :: js-term CountDownList) = (js-expr
	[[
		(function() {
			var i = in, l = [];
			while (i-- >= 0) {
				l.push(i);
			}
			return l;
		})()
	]]
	(= "in" in)
	(type (ListAsArray NatAsNumber))
	(spec in)
))
```

The main purpose of this type of converter is when there is one part of the code that represents `List Nat` as a `ListAsArray NatAsNumber` and another part that represents `List Nat` as `CountDownList` and there is a case where we need to convert between.

As an aside, because of the details of our current implementations of these various functions and of how `metacompiler` works, the following is probably more efficient than the implementation that the compiler would infer for `allNatsLessThan a`:

```
(ConvertCountDownList (CountDownAllNatsLessThan a))
```

