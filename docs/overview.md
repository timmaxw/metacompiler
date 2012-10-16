# Overview

`metacompiler` is an experimental compiler from SL, which is a simple Haskell-like language, to Javascript. It is unusual in that the compiler doesn't have enough built-in knowledge to translate SL into Javascript; instead, the user must provide a "translation" file that specifies how to translate different SL terms into Javascript expressions.

`metacompiler` takes two inputs: SL files, and translation files. The SL files define functions and types but do not specify how to express them as Javascript. The translation files specify ways to translate the functions and types from SL to Javascript. They also specify what should actually be included in the output. The output from `metacompiler` is a Javascript file which defines some functions or values.

## SL

SL, which stands for "specification language", is a simple S-expr-based language that is equivalent to a subset of Haskell. It is documented in the file `SL.md`.

## Translation

The translation process works by means of an intermediate language, the "translation language". Each type and term in the translation language has both a Javascript equivalent and an SL equivalent. A translation file specifies a series of translations, which are types or terms in the translation language. When the compiler is asked to translate a SL term into Javascript, it constructs a translation-language term that is equivalent to the SL term and then emits the Javascript equivalent of that translation-language term.

### A simple example

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
	(impl javascript)
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

### Translation-language types

A translation-language type has the following structure:

```
(<name> <parameter>*)
```

where `<name>` is the name of the `translation` directive that defines the type, and `<parameter>` is another translation-language type or term. If the number and types of the parameters don't match the `translation` directive, the type is malformed.

Every translation-language type is equivalent to some SL type.

Because Javascript is an untyped language, there is no such thing as the Javascript equivalent of a type, but in the future it may be possible to specify a "verifier function" for a translation-language type; the verifier function would be a piece of Javascript code that returned `true` if its input were a valid value for the translation-language type and `false` if not. The compiler would incorporate it into an automatic verifier to check the validity of Javascript equivalents of translation-language terms, but would not use it in production.

### Translation-language terms

A translation-language term has the same structure as a translation-language type.

Every translation-language term is equivalent to some SL term. In addition, every translation-language term has a translation-langauge type and a Javascript equivalent.

### Translation directives

The structure of a translation directive is as follows:

```
(translation <declaration>
	<clause>*
)
```

A declaration is of the form `<name> (<declaration>)* :: <decl-type>`. `<name>` is the name of the thing being declared. The sub-declarations in parentheses after the name are parameters. `<decl-type>` can be either `(type)` or `(term <TL-type>)`, where `<TL-type>` is a translation-language type.

The clauses in the `translation` directive body can be in any order. The details of the clauses depend on whether the declaration declares a type or a term.

### Type translation directives

A type translation directive has two clauses, both of which are mandatory:

#### The `spec` clause

The `spec` clause specifies the SL type which is equivalent to the translation-language type. It has the form:

```
(spec <SL-type>)
```

`<SL-type>` can refer to parameters of the `translation` directive by name. For example:

```
(x ListAsArray (t :: (type)) :: (type)
	(sl (List t))
	...
)
```

Here, the `t` parameter is automatically available in scope within the `spec` clause. Its value is the SL equivalent type of the translation-language type `t`.

#### The `impl` clause

The `impl` clause specifies how to implement this 


