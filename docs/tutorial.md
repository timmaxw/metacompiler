# A simple example: translating Peano numbers into Javascript numbers

Suppose that we have the following SL definitions:

```
(data (Nat) = (Zero) (Succ Nat))
(def (plus (a :: Nat) (b :: Nat)) = (case a of
	Zero -> b
	(Succ x) -> (Succ (plus x b))
	)
)
```

Ignoring integer overflow, a natural translation is to express `Nat` as a Javascript number and `plus` as Javascript addition. We can define those translations as follows:

```
(translation NatAsNumber :: (type)
	(spec (Nat))
)

(translation NatAsNumberZero :: (term NatAsNumber)
	(spec (Zero))
	(impl "0")
)

(translation NatAsNumberSucc (a :: (term NatAsNumber)) :: (term NatAsNumber)
	(spec (Succ a))
	(impl "a + 1" ("a" = a))
)

(translation NatAsNumberPlus (a :: (term NatAsNumber)) (b :: (term NatAsNumber)) :: (term NatAsNumber)
	(spec (plus a b))
	(impl "a + b" ("a" = a) ("b" = b))
)
```

Now, if `metacompiler` is asked to compile the following SL term:

```
(plus (Succ Zero) (Succ Zero))
```

it will produce the following translation-language term:

```
(NatAsNumberPlus (NatAsNumberSucc (NatAsNumberZero)) (NatAsNumberSucc (NatAsNumberZero)))
```

which will become the following Javascript:

```
(((0) + 1) + ((0) + 1))
```

# `Either`

SL definition:

```
(data (Either (l :: *) (r :: *)) = (Left l) (Right r))
```

Translations:

```
(translation EitherAsPair (l :: (type)) (r :: (type)) :: (type)
	(spec (Either l r))
)

(translation EitherAsPairLeft (l :: (type)) (r :: (type)) (x :: (term l)) :: (term (EitherAsPair l r))
	(spec (Left l r x))
	(impl "['left', x]" ("x" = x))
)

(translation EitherAsPairRight (l :: (type)) (r :: (type)) (x :: (term r)) :: (term (EitherAsPair l r))
	(spec (Right l r x))
	(impl "['right', x]" ("x" = x))
)

(translation EitherAsPairCase
		(l :: (type)) (r :: (type)) (res :: (type))
		(subject :: (term (EitherAsPair l r)))
		(leftClause  (value :: (term l)) :: (term res))
		(rightClause (value :: (term r)) :: (term res))
		:: (term res)
	(spec (case subject of
		(Left x) -> (leftClause x)
		(Right x) -> (rightClause x)
	))
	(impl "(function(s) { if (s[0] == 'left') { return leftClause; } else { return rightClause; } })(subject)"
		("subject" = subject)
		("leftClause"  = leftClause  ("s[1]"))
		("rightClause" = rightClause ("s[1]"))
	)
)
```

# Functions

`metacompiler` does not have built-in knowledge of functions. The user must specify how to the SL concept of a function into Javascript. Here is a typical such translation:

```
(translation FunctionType (a :: (type)) (r :: (type)) :: (type)
	(spec (fun a -> r))
	(verifier "typeof it == 'function'" ("it" = it))
)

(translation FunctionLambda (a :: (type)) (r :: (type)) (body (x :: (term a)) :: (term r)) :: (term (FunctionType a r))
	(spec (\ x -> body))
	(impl "function(x) { return body; }"
		(free "x")
		("body" = body ("x"))
	)
)

(translation FunctionApply (a :: (type)) (r :: (type)) (fun :: (term (FunctionType a r))) (arg :: (term a)) :: (term r)
	(spec (fun arg))
	(impl "fun(arg)"
		("fun" = fun)
		("arg" = arg)
	)
)
```
