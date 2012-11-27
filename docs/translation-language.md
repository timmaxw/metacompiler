# Translation language

This document is a detailed specification for the translation files that `metacompiler` takes as input. It assumes you are already familiar with SL. Also, you are advised to "get the hang of" translation files by reading some examples before trying to read this dry document.

`metacompiler` is scripted and controlled by means of a language called the "translation language" or "TL". Technically, TL is a non-Turing-complete dependently-typed functional programming language, but its sole purpose is to script `metacompiler` so it isn't a very powerful or interesting language on its own.

For the purposes of this document, we will introduce some terminology that isn't directly useful to a `metacompiler` user:

 *  A "meta-object" is a value in the translation language.

 *  A "meta-type" is the type of a meta-object.

## Meta-types

The following meta-types exist in the translation language:

### The meta-type of Javascript type representations: `js-type`

```
'js-type'
```

A meta-object of meta-type `js-type` represents a way to represent a SL type in Javascript. It has an SL equivalent, which is an SL type.

### The meta-type of Javascript terms: `js-term` and `js-sl-term`

```
'js-term' (<type>)
'js-sl-term' (<type>)
```

A meta-object of meta-type `js-term` represents a Javascript term. The Javascript term represents an SL value, which has type . A meta-object of meta-type `js-sl-term` 

This is the meta-type of unparameterized translation-language terms which have Javascript equivalents. `<type>` is, naturally, a meta-object that has meta-type `js-type`. The parentheses can be omitted if `<type>` is one word.

### The meta-type of parameterized meta-objects: `fun`

```
'fun' (<name> '::' <meta-type>)+ '->' <meta-type>
```

This is the meta-type of meta-types parameterized on other meta-types. Note that dependent types are possible.

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

The order of `let` directives in the file does not matter; a name defined by a `let` directive is visible everywhere in the file.

## `js-repr` directives

A `js-repr` directive describes a way in which some SL type can be represented in Javascript. It introduces a new meta-object into scope with meta-type `type` or `fun <...> -> type`, where `<...>` is one or more arguments.

A `js-repr` directive looks like this:

```
('js-repr' <name> (<name> '::' <meta-type>)* '='
	('spec' <SL-type>)
)
```

The things after the `=` sign are called "clauses". Right now there's only one clause, but in the future there will probably be more.

The `spec` clause specifies the SL equivalent of the newly created type.

## Meta-object `js-expr` terms

`js-expr` is a special meta-object form that only produces terms, not types. It looks like this:

```
('js-expr' "<JS-code>"
	('type' <type>)
	('spec' <SL-term>)?
	('=' "<JS-var>" <expr>)*
)
```

The things after `"<JS-code>"` are called clauses and they can go in any order.

Its meta-type is `js-term <type>`. Its SL equivalent is `<SL-term>`, if `<SL-term>` is present. Naturally, the SL equivalent of `<type>` must be the type of `<SL-term>`.

Its Javascript implementation is `<JS-code>`, but with any variable whose name appears in a `=` clause replaced with the Javascript equivalent of the `<expr>` in the `=` clause. In addition, any variables that are bound within the Javascript block itself will be replaced with new, unique variable names. For example, `function (x) { return (x + y); }` might become `function (x_1) { return (x_1 + y); }`.

## `use` directives and meta-object `infer` terms

`infer` is a special meta-object form that only produces terms. It looks like this:

```
('infer'
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
('emit' "<JS-code>" ('=' "<JS-var>" <expr>)*)
```

`metacompiler` will perform substitutions on `<JS-code>` as described in the `js-expr` section, and then write the result to the file at the top level.

