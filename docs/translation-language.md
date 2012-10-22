# Translation language

This document is a detailed specification for the translation files that `metacompiler` takes as input. It assumes you are already familiar with SL. Also, you are advised to "get the hang of" translation files by reading some examples before trying to read this dry document.

`metacompiler` translates SL to Javascript by means of an intermediate language, the "translation language". Each type and term in the translation language has both a Javascript equivalent and an SL equivalent. A translation file contains a series of translation directives, which bring into scope types or terms in the translation language. When the compiler is asked to translate a SL term into Javascript, it constructs a translation-language term that is equivalent to the SL term and then emits the Javascript equivalent of that translation-language term.

## Meta-objects and meta-types

For the purposes of this document, we will introduce some terminology that isn't directly useful to a `metacompiler` user:

 *  A "meta-object" is a translation-language type or a translation-language term, parameterized on zero or more meta-objects.

 *  A "meta-type" is the type of a meta-object: How many parameters does it take? What are their meta-types? Is the result a type or a term, and if it is a term, what is that term's type?

## Meta-types

The following syntactic forms are for meta-types:

### The meta-type of unparameterized types: `type`

```
'type'
```

This is the meta-type of unparameterized translation-language types.

### The meta-type of unparameterized terms: `term`

```
'term' (<type>)
```

This is the meta-type of unparameterized translation-language terms. `<type>` means, naturally, a meta-object that has meta-type `type`. The parentheses can be omitted if `<type>` is one word.

### The meta-type of parameterized meta-objects: `fun`

```
'fun' (<meta-type>)+ '->' <meta-type>
```

This is the meta-type of meta-types parameterized on other meta-types. The parentheses can be omitted if `<meta-type>` is one word.

## Meta-object common terms

The following syntactic forms can be used with both types and terms.

### Application

```
<meta-object> (<meta-object>)
```

This provides a value for the first parameter of the meta-object, if any. The first meta-object must have meta-type `fun a -> r` for some `a` and `r`, and the second must have meta-type `a`. The resulting meta-object will have meta-type `r`.

The parentheses can be omitted if `<meta-object>` is only one word.

Its SL equivalent and/or Javascript equivalent are the same as whatever the first meta-object evaluates to after its parameter has been filled.

### Abstraction

```
'\' (<name> '::' (<meta-type>))+ '->' <meta-object>
```

This parameterizes a meta-object on one or more variables. In the one-variable case, if the meta-type is `a` and the meta-object has meta-type `r`, then the resulting meta-object will have meta-type `fun a -> r`.

The parentheses around the `<meta-type>` after the `::` can be omitted if `<meta-type>` is only one word. Parameterization binds more loosely than applying a parameter; `\\ a -> b c` is parsed as `\\ a -> (b c)`.

This has no Javascript equivalent. If `r` is `type` and all of the parameters' meta-types can be represented as SL kinds, then its SL equivalent is an SL type. If `r` is `term <...>`, and all of the parameters' meta-types can be represented as SL kinds or types, and all of the type parameters come before all of the term parametes, then its SL equivalent is a function taking various parameters and returning the SL equivalent of its return value. Otherwise, it cannot be represented in SL.

### Variable reference

```
<name>
```

This refers to a global definition or a variable in scope. The SL equivalent and/or Javascript equivalent are the same as whatever it resolves to.

## `let` directives

A `let` directive binds a meta-object to a name. It looks like this:

```
('let' 'use'? <name> (<name> '::' <meta-type>)* {'::' (<meta-type>)}? '='
	<meta-object>
)
```

It's pretty straight-forward.

The parentheses around the optional `<meta-type>` after the `::` can be omitted if it is only one word.

The optional `'use'` is equivalent to putting a `('use' <name>)` directive after the `let` directive.

The name brought into scope by a `let` directive is only visible after the `let` directive in the file.

## `js-repr` directives

A `js-repr` directive describes a way in which some SL type can be represented in Javascript. It introduces a new meta-object into scope with meta-type `type` or `fun <...> -> type`, where `<...>` is one or more arguments.

A `js-repr` directive looks like this:

```
('js-repr' <name> (<name> '::' <meta-type>)* ':'
	('spec' <SL-type>)
)
```

The things after the `=` sign are called "clauses". Right now there's only one clause, but in the future there will probably be more.

The `spec` clause specifies the SL equivalent of the newly created type.

## Meta-object `js-expr` terms

`js-expr` is a special meta-object form that only produces terms, not types. It looks like this:

```
('js-expr' ':'
	('type' <type>)
	('spec' <SL-term>)
	('impl' <Javascript-block>)
)
```

where `<Javascript-block>` looks like

```
"<Javascript-code-string>" {':' ('set' "<Javascript-var>" <term>)* ('free' "<Javascript-var>")*}?
```

The things after `js-expr` are called clauses and they can go in any order. The things that come after "<Javascript-code-string>" can also be reordered arbitrarily.

Its meta-type is `term <type>`, where `<type>` is the `<type>` from the `type` clause. Its SL equivalent is `<SL-term>`. Naturally, the SL equivalent of `<type>` must be the type of `<SL-term>`.

Its Javascript implementation is `Javascript-code-string`, but with certain variables substituted:

  * `set` means to substitute the `<Javascript-var>` with the Javascript equivalent of the `<term>`. As a special case, within `<term>`, literal strings will evaluate to terms whose Javascript equivalents are those literal strings, and whose SL equivalents are undefined.

  * `free` means to substitute the `<Javascript-var>` with a unique new variable.

## `use` directives and meta-object `infer` terms

`infer` is a special meta-object form that only produces terms. It looks like this:

```
('infer' :
	('goal' <SL-term>)
	('type' <type>)
)
```

When `metacompiler` encounters an `infer` term, it tries to construct a translation-language term whose type is the given type and whose SL equivalent is the given SL term. The `infer` term evalutes to that term.

When inferring, `metacompiler` will only use terms that have been marked with a `use` directive. A `use` directive looks like this:

```
('use' <meta-object>)
```

This enters `<meta-object>` into `metacompiler`'s lookup table, indexed by its SL equivalent. `metacompiler` uses the lookup table to construct equivalent terms.

## `emit` directives

An `emit` directive determines what `metacompiler` will actually write to the output file. An `emit` directive looks like this:

```
('emit' <Javascript-block>)
```

where `<Javascript-block>` is defined as before. `metacompiler` will take the Javascript block, perform the requested substitutions on it, and then write it to the output file.

