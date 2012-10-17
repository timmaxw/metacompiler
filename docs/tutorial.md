# A simple example: translating Peano numbers into Javascript numbers

Suppose that we have the following SL definitions:

```
(data Nat = (Zero) (Succ Nat))
(let plus (a :: Nat) (b :: Nat) :: Nat = case a of
	Zero -> b
	(Succ x) -> (Succ (plus x b))
)
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

(emit "two = x" :
	(set "x" (infer
		(plus (Succ Zero) (Succ Zero))
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

# `Either`

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

# Functions

`metacompiler` does not have built-in knowledge of Javascript functions. The user must specify how to translate the SL concept of a function into Javascript. Here is a typical such translation:

```
(js-repr FunctionType (a :: type) (r :: type) :
	(spec (fun a -> r))
)

(let use FunctionLambda (a :: type) (r :: type) (body :: fun (term a) -> term r) = (js-expr :
	(type (FunctionType a r))
	(spec (\\ x -> body x))
	(impl "function(x) { return body; }" :
		(free "x")
		(set "body" (body "x"))
	)
))

(let use FunctionApply (a :: type) (r :: type) (fun :: term (FunctionType a r)) (arg :: term a) = (js-expr :
	(type r)
	(spec (fun arg))
	(impl "fun(arg)"
		(set "fun" fun)
		(set "arg" arg)
	)
))
```
