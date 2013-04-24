# TL

TL is a domain-specific language used to specify JavaScript translations for SL constructs. The letters "TL" are short for "translation language".

This file is a detailed description of TL syntax and semantics. It assumes basic familiarity with the examples given in `overview.md`.

TL can be thought of as a dependently-typed pure functional language that is not Turing-complete. Like SL, TL has a syntax based on S-expressions; see `s-expressions.md` for a detailed description of those S-expressions.

TL does not allow user-defined types, and it has no types for booleans or numbers. Instead, it has types for SL types and terms, JavaScript expressions, and so on.

## Representing SL types in TL

`'sl-type' "<sl-kind>"` is the type of SL types. The `<sl-kind>` is a string containing an SL kind; this is the kind of the SL type. For example, `(sl-type "*")` is the type of SL types having SL kind `*`.

To create an object of type `(sl-type ...)`, use the following form:

```
('sl-type' "<sl-type>" ('type' "<name>" (<param-name> '::' 'sl-type' "<sl-kind>")* '=' <value>)*)
```

The `<sl-type>` is a SL string representing the type. For example, `(sl-type "Nat")` is the TL representation of the SL type `Nat`. The object `(sl-type "Nat")` has type `(sl-type "*")`.

The optional `(type ... = ...)` clauses after the string are types to be substituted into the type. For example, suppose that the variable `a` has type `(sl-type "*")`. The TL expression for the SL type of a "list of `a`s" would be

```
(sl-type "List x" (type "x" = a))
```

In the SL type `List x`, the `x` will be replaced with whatever the value of `a` is.

It is also possible to pass parameters to the substitutions. This is only for the sake of consistency with `sl-term`, which is described below; there are no actual use cases for it. The best way to explain the syntax is by example. The following expression:

```
(sl-type "List (x Nat)"
	(type "x" (y :: sl-type "*") =
		(sl-type "List z" (type "z" = y))
	)
)
```

is equivalent to:

```
(sl-type "List (List Nat)")
```

## Representing SL terms in TL

`'sl-term' <sl-type>` is the type of SL terms. `<sl-type>` is an object of type `(sl-type "*")` which represents the type of the SL term. For example, `(sl-term (sl-type "Nat"))` is the type of SL terms of type `Nat`.

To create an object of type `sl-term ...`, use the following form:

```
('sl-term' "<sl-term>"
	('type' "<name>" (<param-name> '::' 'sl-type' "<sl-kind>")* '=' <value>)*
	('term' "<name>" (<param-name> '::' 'sl-type' "<sl-kind>")* (<param-name> '::' 'sl-term' <sl-type>)* '=' <value>)*
)
```

This is much like the `(sl-type ...)` expression form, except that it produces a SL type instead of a SL term. Both types and terms can be substituted in. Although the syntax given above shows all the `type` substitutions before all of the `term` substitutions, this is not required.

Here are some examples:

* `(sl-term "Zero")` is an object of type `(sl-type "Nat")`.

* `(sl-term "Succ x" (term "x" = (sl-term "Zero")))` is an object of type `(sl-type "Nat")`. It is equivalent to `(sl-term "Succ Zero")`.

* The following expression:
    
    ```
	(sl-term [[\ (x :: Nat) -> f x]]
		(term "f" (y :: sl-term (sl-type "Nat")) =
			(sl-term "Succ z" (term "z" = y))
		)
	)
	```

	is an object of type `(sl-type "fun Nat -> Nat")`. It is equivalent to `(sl-term [[\ (x :: Nat) -> Succ x]])`. (Note that the `[[` and `]]` are used to delimit the strings instead of `"` and `"` to avoid having to escape the backslash.)

The ability to pass parameters to substitutions wasn't very useful for types. But for terms, it's sometimes absolutely essential. Consider the following example:

```
(sl-term [[\ (x :: Nat) -> f x]]
    (term "f" (y :: sl-term (sl-type "Nat")) = (someFunction y)
)
```

Passing the parameter `x` to `f` is the only way to get a TL representation of `x`, because SL scopes are not transferrable across different `sl-term` literals. For example, the following attempt doesn't work:

```
(sl-term [[\ (x :: Nat) -> f]]
    (term "f" = (someFunction (sl-term "x")))
)
```

This would complain that the variable `x` was not in scope, because the scope from the outer `sl-term` is not transferred to the inner `sl-term`.

In general, `(sl-term ...)` is smart; it preserves semantics rather than syntax. For example, consider the following expression:

```
(sl-term [[\ (x :: Nat) -> f x]]
	(term "f" (y :: sl-term (sl-type "Nat")) =
		(sl-term "x z" (term "z" = x))
	)
)
```

In the expression `x z`, the variable `z` is set by the term-substitution, but the variable `x` is taken from the global SL scope. It is not taken from the function scope of the enclosing `(sl-term ...)` construct. So this will reduce to:

```
(sl-term [[\ (x' :: Nat) -> x x']])
```

to correctly preserve the semantics of the expressions.

## TL functions

TL supports single-parameter pure functions, just like Haskell. All TL functions are total; this is easy because TL is not Turing-complete. Therefore, it is meaningless to ask whether TL functions are strict or lazy.

The type of a TL function is written in the following form:

```
('fun' (<param-name> '::' <type>)+ '->' <type>)
```

For example, the type of functions from two `(sl-type "*")`s to a `(sl-type "*")` could be written as:

```
(fun (x :: sl-type "*") (y :: sl-type "*") -> sl-type "*")
```

Supplying multiple parameters to a function is a syntactic sugar for nested single-parameter functions. So, the above is equivalent to:

```
(fun (x :: sl-type "*") -> (fun (y :: sl-type "*") -> sl-type "*"))
```

Note that even when talking about the type of a function, the parameters to the function must be given names. This is because functions can be dependently-typed. For example, the type of functions that take some SL type and return SL terms of that type could be written as:

```
(fun (x :: sl-type "*") -> sl-term x)
```

To create TL functions, use the following syntax:

```
('\' (<param-name> '::' <type>)+ '->' <body>)
```

Here are some examples:

* `(\ (x :: sl-type "*") -> (sl-type "List a" (type "a" = x)))` is a function which wraps the given SL type in `List`. It has type `(fun (x :: sl-type "*") -> sl-type "*")`.

* `(\ (x :: sl-type "*") -> sl-term "undefined . a" (type "a" = x))` is a function which, given a SL type, returns `undefined` of that type. It has type `(fun (x :: sl-type "*") -> sl-type "*")`.

Functions can be called in the same way as in Haskell. For example, the following expression:

```
(\ (x :: sl-type "*") -> (sl-type "List a" (type "a" = x))) (sl-type "Nat")
```

is just equivalent to `(sl-type "List Nat")`.

## JavaScript type representations

`'js-expr-type' <sl-type>` is the type of JavaScript representations of the given SL type. For example, `js-expr-type (sl-type "Nat")` is the type of JavaScript representations of `Nat`.

The only way to create objects of type `(js-expr-type ...)` is using the top-level `js-expr-type` directive, which takes the following form:

```
('js-expr-type' <name> (<param-name> '::' <type>)* '='
	('spec' <sl-type>)
)
```

This introduces a new object into scope under the name `<name>`. If there are no parameters, then `<name>` has type `(js-expr-type x)`, where `x` is the SL type given in the `spec` clause. If one or more parameters are given, then `<name>` will be a function that ultimately returns `(js-expr-type x)`.

For example, the following creates a JavaScript representation of `Nat`:

```
(js-expr-type NatAsNumber = (spec (sl-type "Nat")))
```

The name `NatAsNumber` is meant to suggest that the `Nat` will be represented as a JavaScript number. However, there is nothing in the `js-expr-type` that formally specifies that mapping. Rather, it is implicitly specified in how functions that manipulate `NatAsNumber` objects are implemented.

Here's another example of a `js-expr-type` declaration with parameters:

```
(js-expr-type ListAsArray (elemTypeSL :: sl-type "*") (elemTypeJS :: js-expr-type elemTypeSL) =
	(spec (sl-type "List e" (type "e" = elemTypeSL)))
)
```

Here, `elemTypeSL` is the SL type of the list's elements, and `elemTypeJS` is the JavaScript representation that will be used for that SL type. So, `ListAsArray (sl-type "Nat") NatAsNumber` is a JavaScript representation for the SL type `List Nat`, and it means to represent the list of `Nat`s as a JavaScript array of JavaScript numbers.

## JavaScript term representation

`'js-expr' <js-expr-type> <sl-term>` is the type of JavaScript expressions. There are two extra pieces of information attached to the JavaScript expression type. The `<js-expr-type>` is the SL-to-JavaScript representation under which the expression is being interpreted. For example, a JavaScript expression of type `js-expr NatAsNumber ...` should evaluate to a JavaScript number. The `<sl-term>` is the actual SL term which the JavaScript expression is equivalent to under the representation. For example, a JavaScript expression of type `js-expr NatAsNumber (sl-term "Zero")` should evaluate to the JavaScript number `0`. Naturally, the SL equivalent of the `<js-expr-type>` must be the same as the SL type of the `<sl-term>`; in this case, they would both be `Nat`.

`(js-expr ...)` objects can be created using the following syntax:

```
('js-expr'
	('type' <js-expr-type>)
	('spec' <sl-term>)
	('impl' "<JavaScript-expression>"
		('expr' "<JavaScript-variable>"
			(<param-name-1> '::' 'sl-term' <sl-type> '|' <param-name-2> '::' 'js-expr' <js-expr-type> <param-name-1>)*
			'=' <value>
		)*
	)
)
```

The `(type ...)` and `(spec ...)` clauses specify the SL-to-JavaScript representation and SL equivalent for the JavaScript expression. The `<JavaScript-expression>` given in the `(impl ...)` clause is the actual JavaScript expression itself. For example:

```
(js-expr (type NatAsNumber) (spec (sl-term "Zero")) (impl "0"))
```

has type `(js-expr NatAsNumber (sl-term "Zero"))`, and evaluates to the JavaScript expression `0`.

The `(expr ...)` clauses can be used to substitute other JavaScript expressions into the expression. For example:

```
(js-expr
	(type NatAsNumber)
	(spec (sl-term "Succ x" (term "x" = numberSL)))
	(impl "y() + 1" (expr "y" = numberJS))
)
```

evaluates to the the JavaScript expression "<?> + 1", where `<?>` is `numberJS`. Note that the substitution must be invoked like a function even when it takes no parameters. It would make the most sense for `numberJS` to have type `js-expr NatAsNumber numberSL`, although `metacompiler` will only enforce that `numberJS` has type `(js-expr a b)` for some `a` and `b`.

The JavaScript expression substitutions can themselves get parameters back from JavaScript. The bizarre `(name1 :: sl-term ... | name2 :: js-expr ... name1)` syntax is to bring both the SL and JavaScript representations of the parameter into scope. Consider the following example:

```
(js-expr
	(type (FunctionType Nat Nat))
	(spec (sl-term "\ (x :: Nat) -> f x"
		(term "f"
			(param :: sl-term (sl-type "Nat"))
			= functionBodySL param
		)
	))
	(impl "function (x) { return f(x); }"
		(expr "f"
			(paramSL :: sl-term (sl-type "Nat") | paramJS :: js-expr NatAsNumber paramSL)
			= functionBodyJS paramSL paramJS
		)
	)
)
```

Here, `functionBodySL` is a function of type

```
fun (param :: sl-term (sl-type "Nat")) -> sl-term (sl-type "Nat")
```

and `functionBodyJS` is a function of type

```
fun (paramSL :: sl-term (sl-type "Nat")) (paramJS :: js-expr NatAsNumber paramSL) -> js-expr NatAsNumber (functionBodySL paramSL)
```

When the `js-expr` is evaluated, `paramSL` will be a SL term which corresponds to the value of the JavaScript variable `x`. The JavaScript variable `x` corresponds to the SL variable `x` in the `(spec ...)` clause, but `metacompiler` isn't smart enough to figure that out; `paramSL` is just an arbitrary opaque value. `paramJS` is the actual JavaScript variable `x`.

Like `(sl-term ...)`, `(js-expr ...)` will rename variables as necessary when substituting to preserve the semantics of the JavaScript expressions.

Note that `metacompiler` will take "on faith" that the given JavaScript code actually means what the user claims that it means. For example, consider the following expression:

```
(js-expr (type NatAsNumber) (spec (sl-term "Zero")) (impl "1"))
```

This `(js-expr ...)` claims to use the representation `NatAsNumber` and to be equivalent to the SL term `Zero`, so its JavaScript code should evaluate to the JavaScript number `0`. But instead, it evaluates to `1`. This is wrong; but there is no way for `metacompiler` to know that, so it will accept this code without complaint.

## `js-expr-loop-break` and `js-expr-convert-spec`

These are not documented yet because they are likely to change in the near future.

## `let` directives

Global definitions can be created with the top-level `let` directive. The syntax is:

```
('let' <name> (<param-name> '::' <type>)* {'::' <type>}? '=' <value>)
```

If no parameters are provided, this simply binds `<name>` to `<value>`; if parameters are provided, then it binds `<name>` to a function of that takes those parameters and returns `<value>`. If the optional `'::' <type>` is present after the parameters, it must be the same as the type of `<value>`.

Here's an example:

```
(let MakeUndefined (x :: sl-type "*") :: (sl-term x) = (sl-term "undefined . a" (type "a" = x)))
```

This will define `MakeUndefined` to be:

```
(\ (x :: sl-type "*") -> (sl-term "undefined . a" (type "a" = x)))
```

Global definitions can be in any order, but they may not be recursive.

## `sl-code` directives

`sl-code` directives allow you to embed some SL code in your TL file. The types, constructors, and values defined in the SL code will be visible in every `(sl-type ...)` and `(sl-term ...)` expression in the TL file.

The syntax is:

```
('sl-code' "<sl-directives>")
```

For example, to bring SL's `Nat` and `plus` into scope, one could write:

```
(sl-code [[

	(data Nat = (Zero) (Succ Nat))

	(let plus . (x :: Nat) (y :: Nat) :: Nat =
		(case x of
			(Zero .) -> y
			(Succ . x') -> (Succ . (plus . x' y))
		)
	)

]])
```

Currently, there is no SL module system, so this is the only way to bring SL code into scope.

## `js-emit` directives

A `js-emit` directive is the way to actually emit JavaScript code. The syntax is:

```
('js-emit' "<JavaScript-statements>"
	('expr' "<JavaScript-variable>"
		(<param-name-1> '::' 'sl-term' <sl-type> '|' <param-name-2> '::' 'js-expr' <js-expr-type> <param-name-1>)*
		'=' <value>
	)*
)
```

The string immediately after `js-emit` is what will be emitted. The `(expr ...)` clauses can be used to substitute `(js-expr ...)` objects into the JavaScript code to be emitted.

