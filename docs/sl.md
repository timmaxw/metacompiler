# SL

SL is a lazy pure functional programming language that is equivalent to a subset of Haskell. Its syntax is expressed in S-expressions; see `s-expressions.md` for a description of those S-expressions.

"SL" is short for "specification language", because it is used to specify the semantics of the program.

## Kinds

Kinds in SL are the same as in Haskell 98, but with slightly different syntax.

The kind of ordinary types is written as `*`. The kind of a type constructor is written in the following form:

```
'fun' (<kind>)+ '->' <kind>
```

For example, the kind of the type-constructor `Maybe` could be written as `fun * -> *`.

When multiple kinds are specified between `fun` and `->`, this is a syntactic sugar. For example, the kind of the type-constructor `Either` could be equivalently written either as `fun * * -> *` or as `fun * -> (fun * -> *)`.

## Types

The type system is also much like Haskell 98's, but with different syntax, fewer built-in types, and a special syntax for laziness.

Named types can (obviously) be referred to by name, and application of type constructors is as in Haskell. For example, `(List Nat)` is the type of lists of natural numbers.

Function types are written in the same form as the kinds of type constructors. For example, the type of binary functions on natural numbers is written as `fun Nat Nat -> Nat`. Of course, this is syntactic sugar for `fun Nat -> (fun Nat -> Nat)`. More complicated types are also possible; for example, the type of `map` for two arbitrary types `a` and `b` would be `fun (fun a -> b) (List a) -> List b`.

Functions in SL are strict in their parameters. To get a lazy value, use the following special type:

```
'lazy' <type>
```

This gives a type which holds an unevaluated value. For example, consider the following Haskell function:

```
if' :: Bool -> a -> a -> a
if' True x y = x
if' False x y = y
```

This function is much more useful if `x` and `y` can be passed lazily. In SL, its type could be written as:

```
fun Bool (lazy a) (lazy a) -> a
```

## User-defined data types

SL supports top-level `data` directives, as in Haskell, to introduce new types into scope. The general form of these is:

```
('data' <name> (<param-name> '::' <kind>)* '=' (<ctor-name> <type>*)+)
```

This introduces a new type into scope, with the given name. It takes one or more type parameters, the names and kinds of which must be specified explicitly, unlike in Haskell. It has one or more constructors, each of which has zero or more fields.

For example, the type of natural numbers could be defined as follows:

```
(data Nat = (Zero) (Succ Nat))
```

Or, the type `Maybe` can be defined as:

```
(data Maybe (a :: *) = (Nothing) (Just a))
```

There are no restrictions on capitalization of the name of the type or its parameters; unlike in Haskell, either can begin with a capital or lower-case letter.

Unlike in Haskell, all fields are strict.

## Expressions

Expressions take the following forms:

### Name references

Non-polymorphic names can be written as in Haskell.

However, SL will not infer the types for polymorphic names; rather, they must be specified explicitly using the following syntax:

```
<name> <type>* '.'
```

For example, the `Nothing` constructor for the type `Maybe Nat` would be written as `(Nothing Nat .)`. 

### Lambdas and function application

Lambdas take the following form:

```
'\' (<name> '::' <type>)+ '->' <expr>
```

For example, the identity function for `Nat` could be written as `(\ (x :: Nat) -> x)`. If multiple parameters are specified, this is a syntactic sugar for nested lambdas, as in Haskell.

Function application is also as in Haskell. For example, `((\ (x :: Nat) -> x) Zero)` will evaluate to `Zero`. When calling a polymorphic function, function parameters can be written after the `.`. For example, to fill the `Just` constructor for the type `Maybe Nat` with the value 2, we could write:

```
(Just Nat . (Succ (Succ Zero)))
```

### `case` expressions

`case` expressions take the following form:

```
'case' (<expr>) 'of' <case>*
```

where `<case>` is

```
(<ctor> <type>* '.' <name>*) '->' (<expr>)
```

For example, the following function returns true if the given `Maybe Nat` is equal to `Just Nat . (Succ (Succ Zero))`, and false otherwise:

```
(\ (x :: Maybe Nat) -> (case x of
	(Nothing Nat .) -> False
	(Just Nat . y) -> (case y of
		(Zero .) -> False
		(Succ . z) -> (case z of
			(Zero .) -> True
			(Succ . w) -> False
			)
		)
	)
)
```

### Lazy values

To "wrap" a value in a lazy type for later evaluation, use the following syntax:

```
'wrap' <expr>
```

To reverse the process:

```
'unwrap' <expr>
```

Naturally, `unwrap (wrap x)` is just equivalent to `x`.

## `let` directives

A `let` directive binds a term to a name. It looks like this:

```
('let' <name> (<param-name> '::' <kind>)* '.' (<param-name> '::' <type>)* '::' <type> '=' <expr>)
```

It binds the name `<name>` to a function of zero or more parameters, evaluating to the thing on the right-hand side of the `=`. The type of the thing on the right-hand side of the `=` must evaluate to the `<type>` after the `::`. This is to simplify type inference.

`let` and `data` directives can refer to names that were bound later in the file. So, order of directives does not matter.

For example, a function to compute the sum of two natural numbers can be defined as follows:

```
(let add (x :: Nat) (y :: Nat) :: Nat = 
	(case x of
		(Zero) -> y
		(Succ x') -> (add x' y)
	)
)
```
