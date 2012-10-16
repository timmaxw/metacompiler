# Translation language

This document is a detailed specification for the translation files that `metacompiler` takes as input. It assumes you are already familiar with SL. Also, you are advised to "get the hang of" translation files by reading some examples before trying to read this dry document.

`metacompiler` translates SL to Javascript by means of an intermediate language, the "translation language". Each type and term in the translation language has both a Javascript equivalent and an SL equivalent. A translation file contains a series of translation directives, which bring into scope types or terms in the translation language. When the compiler is asked to translate a SL term into Javascript, it constructs a translation-language term that is equivalent to the SL term and then emits the Javascript equivalent of that translation-language term.

# Objects, meta-types and declarations

`metacompiler` often uses similar syntax for types and terms. For example, in translation directives, type parameters can be specified next to term parameters. An "object" is either a type or a term, parameterized on zero or more other objects. An object's "meta-type" indicates whether it is a type or a term, what its parameters are, and so on.

Types cannot be parameterized on terms. If terms are parameterized on types, all of the type parameters must come before any of the term parameters.

## Object syntax

The syntax for an type or a term comes in one of two forms.

The first form is used to refer to a translation directive. It is as follows:

```
(<name> ([<var>* '->'] <object>)*)
```

The initial `<name>` is the name of the translation directive in question. The things appearing in parentheses after it are parameters to it. If the parameter itself takes sub-parameters, then those sub-parameters' names are given by the `<var>`s before the `->`. The number of parameters and the meta-types of the parameters must exactly match those of the translation directive; there is no partial application.

Names of translation directives must begin with a capital letter. Names of variables must begin with a lower-case letter.

If there are no parameters, the outer parentheses may be omitted. For a parameter, if there is no `->` clause, and the object within the parentheses is a single word, then the parameter parentheses may be omitted.

The second form is used to refer to a variable. It is as follows:

```
<var>
```

This is pretty obvious.

## Declarations

A "declaration" describes an object's meta-type, and also assigns names to its parameters. The syntax for declarations is:

```
<name> (<declaration>)* '::' <type-or-term-marker>
```

where `<type-or-term-marker>` is either `('type')` or `('term' <TL-type>)`, where `<TL-type>` is a translation-language type.

The declaration indicates that `<name>` has the parameters that appear in parentheses after it. If `<type-or-term-marker>` is `('type')`, then `<name>` is a type; otherwise, it is a term, and its type is `<TL-type>`.

Unlike Haskell's type declarations, our declarations do not appear at the top level of the file. Instead, they appear at the top of translation directives.

## SL equivalents

Every translation-language type has some SL type that is its equivalent. Same with every translation-language term. These are determined by looking at the translation directive that the SL type refers to, and taking the type in its `spec` clause; see the sections on translation directives for detaisl.

Sometimes translation-language objects are bound to variables in SL. The way this translation is accomplished is as follows:

Translation-language types are translated to their SL equivalents. If they are parameterized on types, then they become SL types with kinds other than `*`.

For example, `ListAsArray (x :: (type)) :: (type)` would be represented in SL as a type of kind `(fun * -> *)`.

Translation-language terms are similarly translated. If the translation-language terms are parameterized on types, then their SL equivalents are polymorphic. If they are parameterized on terms, then their SL equivalents become functions.

# Javascript blocks

Sometimes it's necessary to have a block of Javascript and to substitute translation-language terms into it. Here's the syntax for that:

```
"<js-expr-string>" ("<js-var>" '=' <var> (<js-block>)*)* ('free' "<js-var>")*
```

where `<js-block>` another `<js-expr-string>` and parameter list and so on recursively.

The `free`-clauses and `=`-clauses can be mixed in any order.

The Javascript code that this is equivalent to is "<js-expr-string>", with the following changes:

 *  For each `=`-clause, the Javascript variable to the left of the `=` is replaced with the Javascript equivalent of whatever is to the right of the `=`. The thing to the right of the `=` sign can take parameters, in which case those parameters are further Javascript blocks. This allows for function literals, `case`-expressions, and so on to be expressed.
 
 *  For each `free`-clause, the Javascript variable is replaced with a new unique Javascript identifier.

# Type translation directives

A type translation directive describes a way in which some SL type can be represented in Javascript. It introduces a name into scope that, when instantiated with zero or more parameters, forms a translation-language type.

Because Javascript is an untyped language, it's impossible to specify a Javascript equivalent. Instead, the type translation directive just acts as a thing for term translation directives to refer to to inform the compiler that they are using the same Javascript representation for some SL type.

The syntax of a type translation directive is as follows:

```
('translation' <declaration>
	<clause>*
)
```

The declaration gives the name and parameters of the type. Naturally, it must end with `'::' ('type')`.

The clauses give details about the translation. They can be in any order.

## The `spec` clause

The `spec` clause specifies the SL type which is equivalent to the translation-language type. It is mandatory. It has the form:

```
('spec' <SL-type>)
```

Of course, all of the parameters of the declaration are exposed within `<SL-type>`. They are simply bound to the names given in the declaration.

## The `verifier` clause

The `verifier` clause is used to specify a chunk of Javascript code which returns `true` if its input is a valid value of this type and returns something else or throws an exception otherwise. It is optional. It has the form:

```
('verifier' <js-block>)
```

Within `<js-block>`, the variable `it` is bound to the thing being verified. For example, if some SL type were being translated to a Javascript string, the verifier clause could look like this:

```
(verifier "typeof x == 'string'" ("x" = it))
```

The verifier code should have no side effects.

Typically, the verifier code will only be used when `metacompiler` is asked to perform automatic verification; otherwise it will not be used.

# Term translation directives

A term translation directive represents a way in which some SL term can be transformed into a Javascript expression. It introduces a name into scope that, when instantiated with zero or more parameters, becomes a translation-language term.

Naturally, the syntax for term translation directives is very similar to that for type translation directives:

```
('translation' <declaration>
	<clause>*
)
```

But, of course, this time the `<declaration>` must end with `'::' ('term' <TL-type>)`.

## The `spec` clause

The `spec` clause specifies the spec-language equivalent to this translation-language term. It is mandatory. It has the form:

```
('spec' <SL-term>)
```

Just like with type translation directives, any parameters to the term translation directive will be exposed within `<SL-term>`.

## The `impl` clause

The `impl` clause specifies how to translate the term into Javascript. It is mandatory. It takes the following form:

```
('impl' <js-block>)
```

