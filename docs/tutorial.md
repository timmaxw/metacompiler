# Examples

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

(let use NatAsNumberZero = (js-expr :
	(type NatAsNumber)
	(spec Zero)
	(impl "0")
))

(let use NatAsNumberSucc (x :: term NatAsNumber) = (js-expr :
	(type NatAsNumber)
	(spec (Succ x))
	(impl "x + 1" : (set "x" x))
))

(let use NatAsNumberPlus (x :: term NatAsNumber) (y :: term NatAsNumber) = (js-expr :
	(type NatAsNumber)
	(spec (plus x y))
	(impl "x + y" : (set "x" x) (set "y" y))
))

(let use NatAsNumberTimes (x :: term NatAsNumber) (y :: term NatAsNumber) = (js-expr :
	(type NatAsNumber)
	(spec (times x y))
	(impl "x * y" : (set "x" x) (set "y" y))
))

(emit "two = x" :
	(set "x" (infer :
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

## Translating `Either` to Javascript

SL definition:

```
(data Either (l :: *) (r :: *) = (Left l) (Right r))
```

Translations:

```
(js-repr EitherAsPair (l :: type) (r :: type) :
	(spec (Either l r))
)

(let use EitherAsPairLeft (l :: type) (r :: type) (x :: term l) = (js-expr :
	(type (EitherAsPair l r))
	(spec (Left l r x))
	(impl "['left', x]" : (set "x" x))
))

(let use EitherAsPairRight (l :: type) (r :: type) (x :: term r) = (js-expr :
	(type (EitherAsPair l r))
	(spec (Right l r x))
	(impl "['right', x]" : (set "x" x))
))

(let use EitherAsPairCase
		(l :: type) (r :: type) (res :: type)
		(subject :: term (EitherAsPair l r))
		(leftClause :: fun (term l) -> term res)
		(rightClause :: fun (term r) -> term res)
		= (js-expr :
	(type res)
	(spec (case subject of
		(Left x) -> (leftClause x)
		(Right x) -> (rightClause x)
	))
	(impl "(function(s) { if (s[0] == 'left') { return leftClause; } else { return rightClause; } })(subject)" :
		(set "subject" subject)
		(set "leftClause"  (leftClause  "s[1]"))
		(set "rightClause" (rightClause "s[1]"))
		(free "s")
	)
))
```

## Translating functions to Javascript

`metacompiler` does not have built-in knowledge of Javascript functions. The user must specify how to translate the SL concept of a function into Javascript. Here is a typical such translation:

```
(js-repr FunctionType
		(a :: type) (r :: type) :
	(spec (fun a -> r))
)

(let use FunctionLambda
		(a :: type) (r :: type)
		(body :: fun (term a) -> term r)
		= (js-expr :
	(type (FunctionType a r))
	(spec (\\ x -> body x))
	(impl "function(x) { return body; }" :
		(free "x")
		(set "body" (body "x"))
	)
))

(let use FunctionApply
		(a :: type) (r :: type)
		(fun :: term (FunctionType a r)) (arg :: term a)
		= (js-expr :
	(type r)
	(spec (fun arg))
	(impl "fun(arg)"
		(set "fun" fun)
		(set "arg" arg)
	)
))
```

## Multiple-parameter functions

Suppose that we want `metacompiler` to turn SL functions that take multiple parameters (curried, of course) into Javascript functions that take multiple parameters. Here's a way to do that:

```
(js-repr Function2Type
		(a1 :: type) (a2 :: type) (r :: type) :
	(spec (fun a1 a2 -> r))
)

(let use Function2Lambda
		(a1 :: type) (a2 :: type) (r :: type)
		(body :: fun (term a1) (term a2) -> term r)
		= (js-expr :
	(type (Function2Type a1 a2 r))
	(spec (\\ x1 x2 -> body x1 x2))
	(impl "function(x1, x2) { return body; }" :
		(free "x1")
		(free "x2")
		(set "body" (body "x1" "x2"))
	)
))

(let use Function2Apply
		(a1 :: type) (a2 :: type) (r :: type)
		(fun :: term (Function2Type a1 a2 r)) (arg1 :: term a1) (arg2 :: term a2)
		= (js-expr :
	(type r)
	(spec (fun arg1 arg2))
	(impl "fun(arg1, arg2)" :
		(set "fun" fun)
		(set "arg1" arg1)
		(set "arg2" arg2)
	)
))
```

## Lazy values

Just like for functions, `metacompiler` must be taught how to represent lazily computed values:

```
(js-repr LazyType (a :: type) :
	(spec (lazy a))
)

(let use LazyWrap (a :: type) (x :: term a) = (js-expr :
	(type (LazyType a))
	(spec (wrap x))
	(impl "function() { return x; }" :
		(set "x" x)
	)
))

(let use LazyUnwrap (a :: type) (x :: term (LazyType a)) = (js-expr :
	(type a)
	(spec (unwrap x))
	(impl "x()" :
		(set "x" x)
	)
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
(js-repr MaybeAsNull (a :: type) :
	(spec (Maybe a))
)

(let MaybeAsNullNothing (a :: type) = (js-expr :
	(type (MaybeAsNull a))
	(spec (Nothing a))
	(impl "null")
))

(let MaybeAsNullJust (a :: type) (val :: term a) = (js-expr :
	(type (MaybeAsNull a))
	(spec (Just a val))
	(impl "x" (set "x" val))
))

(let MaybeAsNullCase
		(a :: type) (res :: type)
		(subject :: term (MaybeAsNull a))
		(nothingClause :: term res)
		(justClause :: fun (term a) -> term res)
		= (js-expr :
	(type res)
	(spec (case subject of (Nothing) -> (nothingClause) (Just x) -> (justClause x)))
	(impl "(function(s) { if (s == null) { return nc; } else { return jc; }})(subj)" :
		(set "subj" subject)
		(free "s")
		(set "nc" nothingClause)
		(set "jc" (justClause "s"))
	)
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
(let use Factorial = (js-expr :
	(type (FunctionType NatAsNumber NatAsNumber))
	(spec factorial)
	(impl "function(a) { var x = 1; while (a > 0) { x *= a; a--; } return x; }")
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
(js-repr ListAsArray (a :: type) :
	(spec (List a))
)

(let use ListAsArrayNil (a :: type) = (js-expr :
	(type (ListAsArray a))
	(spec Nil)
	(impl "[]")
))

(let use ListAsArrayCons
		(a :: type) (x :: term a) (xs :: term (ListAsArray a))
		= (js-expr :
	(type (ListAsArray a))
	(spec (Cons x xs))
	(impl "[x].concat(xs)" : (set "x" x) (set "xs" xs))
))

(let use ListAsArrayCase
		(a :: type) (res :: type)
		(subject :: term (ListAsArray a))
		(nilClause :: term res)
		(consClause :: fun (term a) (term (ListAsArray a)) -> term res)
		= (js-expr :
	(type res)
	(spec (case subject of (Nil) -> (nilClause) (Cons x xs) -> (ConsClause x xs)))
	(impl "(function(s) { if (s.length == 0) { return nc; } else { return cc; }})(subj)" :
		(set "subj" subject)
		(set "nc" nilClause)
		(set "cc" (consClause "s[0]" "s.slice(1, s.length)"))
		(free "s")
	)
))

(let use ListAsArrayMap
		(a :: type) (b :: type)
		= (js-expr :
	(type (Function2Type (FunctionType a b) (ListAsArray a) (ListAsArray b)))
	(spec map)
	(impl "function(f, l) { var l2 = []; for (var x in l) { l2.push(f(x)); } return l2; }")
))
```

