# Translation language

This document is a detailed specification for the translation files that `metacompiler` takes as input. It assumes you are already familiar with SL. Also, you are advised to "get the hang of" translation files by reading some examples before trying to read this dry document.

`metacompiler` translates SL to Javascript by means of an intermediate language, the "translation language". Each type and term in the translation language has both a Javascript equivalent and an SL equivalent. A translation file contains a series of translation directives, which bring into scope types or terms in the translation language. When the compiler is asked to translate a SL term into Javascript, it constructs a translation-language term that is equivalent to the SL term and then emits the Javascript equivalent of that translation-language term.

# Objects, meta-types and declarations

`metacompiler` often uses similar syntax for types and terms. For example, in translation directives, type parameters can be mixed with term parameters. An "object" is either a type or a term, parameterized on zero or more other objects. An object's "meta-type" indicates whether it is a type or a term, what its parameters are, and so on.

Types cannot be parameterized on terms. If terms are parameterized on types, all of the type parameters must come before any of the term parameters.

## Object syntax

The syntax for an object comes in one of two forms.

The first form is used to refer to a translation directive. It is as follows:

```
<name> ([<var>* ->] <object>)*
```

The initial `<name>` is the name of the translation directive in question. The things appearing in parentheses after it are parameters to it. If the parameter itself takes sub-parameters, then those sub-parameters' names are given by the `<var>`s before the `->`.

The second form is used to refer to a variable. It is as follows:

```
<var>
```

This is pretty obvious.

## Declarations

A "declaration" describes an object's meta-type, and also assigns names to its parameters. The syntax for declarations is:

```
<name> (<declaration>)* :: <type-or-term-marker>
```

where `<type-or-term-marker>` is either `(type)` or `(term <TL-type>)`, where `<TL-type>` is a translation-language type.

The declaration indicates that `<name>` has the parameters that appear in parentheses after it. If `<type-or-term-marker>` is `(type)`, then `<name>` is a type; otherwise, it is a term, and its type is `<TL-type>`.

Unlike Haskell's type declarations, our declarations do not appear at the top level of the file. Instead, they appear at the top of translation directives.

## SL equivalents

Every translation-language type has some SL type that is its equivalent. Same with every translation-language term. These are determined by looking at the translation directive that the SL type refers to, and taking the type in its `spec` clause; see the sections on translation directives for detaisl.

Sometimes translation-language objects are bound to variables in SL. The way this translation is accomplished is as follows:

Translation-language types are translated to their SL equivalents. If they are parameterized on types, then they become SL types with kinds other than `*`.

For example, `ListAsArray (x :: (type)) :: (type)` would be represented in SL as a type of kind `(fun * -> *)`.

Translation-language terms are similarly translated. If the translation-language terms are parameterized on types, then their SL equivalents are polymorphic. If they are parameterized on terms, then their SL equivalents become functions.

# Type translation directives

A type translation directive describes a way in which some SL type can be represented in Javascript. It introduces a name into scope that, when instantiated with zero or more parameters, forms a translation-language type.

Because Javascript is an untyped language, it's impossible to specify a Javascript equivalent. Instead, the type translation directive just acts as a thing for term translation directives to refer to to inform the compiler that they are using the same Javascript representation for some SL type.

The syntax of a type translation directive is as follows:

```
(translation <declaration>
	<clause>*
)
```

The declaration gives the name and parameters of the type. Naturally, it must end with `:: (type)`.

The clauses give details about the translation. They can be in any order.

## The `spec` clause

The `spec` clause specifies the SL type which is equivalent to the translation-language type. It is mandatory. It has the form:

```
(spec <SL-type>)
```

Of course, all of the parameters of the declaration are exposed within `<SL-type>`. They are simply bound to the names given in the declaration.

## The `verifier` clause

The `verifier` clause is used to specify a chunk of Javascript code which returns `true` if its input is a valid value of this type and returns something else or throws an exception otherwise. It is optional. It has the form:

```
(verifier "<js-expr-string>" ("<js-var>" = it))
```

When `<js-expr-string>` is evaluated with `<js-var>` bound to or replaced by the value, it should return `true` if it is valid and return something else or throw an exception if it is not. The verifier code should have no other side effects.

Typically, the verifier code will only be used when `metacompiler` is asked to perform automatic verification; otherwise it will not be used.

# Term translation directives

A term translation directive represents a way in which some SL term can be transformed into a Javascript expression. It introduces a name into scope that, when instantiated with zero or more parameters, becomes a translation-language term.

Naturally, the syntax for term translation directives is very similar to that for type translation directives:

```
(translation <declaration>
	<clause>*
)
```

But, of course, this time the `<declaration>` must end with `:: (term <TL-type>)`.

## The `spec` clause

The `spec` clause specifies the spec-language equivalent to this translation-language term. It is mandatory. It has the form:

```
(spec <SL-term>)
```

Just like with type translation directives, any parameters to the term translation directive will be exposed within `<SL-term>`.

## The `impl` clause

The `impl` clause specifies how to translate the term into Javascript. It is mandatory. It takes the following form:

```
(impl <block>)
```

where `<block>` looks like:

```
"<js-expr-string>" ("<js-var>" = <var> (`<block>`)*)
```

The `<js-expr-string>` of the root block will become the Javascript expression that is equivalent to the term. The `<js-var>`s after it will be bound to the parameters of the term. If the terms themselves take further parameters, those will be bound to the given Javascript expressions, and so on recursively. This allows for complicated constructs like lambdas to be translated.


