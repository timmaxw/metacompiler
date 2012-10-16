# A simple example: translating Peano numbers into Javascript numbers

Suppose that we have the following SL definitions:

```
(data (Nat) = (Zero) (Succ Nat))
(def (plus a b) = (case (a) of
	((Zero) (b))
	((Succ x) (Succ (plus x b)))
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
	(impl javascript () "0")
)

(translation NatAsNumberSucc (a :: (term NatAsNumber)) :: (term NatAsNumber)
	(spec (Succ a))
	(impl javascript ("a" = a) "a + 1")
)

(translation NatAsNumberPlus (a :: (term NatAsNumber)) (b :: (term NatAsNumber)) :: (term NatAsNumber)
	(spec (plus a b))
	(impl javascript ("a" = a) ("b" = b) "a + b")
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
