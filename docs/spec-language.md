# Spec language (SL)

The "spec language" of `metacompiler`, also called SL, is a lazy pure functional programming language that is equivalent to a subset of Haskell. For ease of implementation, it is expressed as S-expressions.

## Kinds

Kinds take the following forms:

```
'*'
```

The kind of inhabited types.

```
'fun' (<kind>)+ '->' <kind>
```

The kind of types that are parameterized on other types. The parentheses can be omitted if the kind is one word.

## Types

Types take the following forms:

### Name references

```
<name>
```

Refers to a variable or definition in scope.

### Providing parameters

```
<type> <type>
```

Provides a value for the parameter of a type.

### Function types

```
'fun' (<type>)+ '->' <type>
```

The type of curried functions. The parentheses can be omitted if the type is one word. All functions in SL are strict!

### Lazy value types

```
'lazy' <type>
```

The type of lazy values.

## Expressions

Expressions take the following forms:

### Name references

```
<name> (<type>)*
```

Refers to a variable or definition in scope. The types are type-parameters for if the name refers to something polymorphic. The parentheses can be omitted if the types are one word.

### Application

```
<expr> (<expr>)
```

Application. The parentheses can be omitted if the parameter is one word.

### Abstraction

```
'\' (<name> '::' <type>)+ '->' <expr>
```

Abstraction.

### Examining data values

```
'case' (<expr>) 'of' <case>*
```

where `<case>` is

```
(<ctor> <name>*) '->' (<expr>)
```

Examining a data value. The parentheses around the `<expr>` after `case` can be omitted if it is only one word. The parentheses around `<ctor>` can be omitted if no `<name>`s follow it. The parentheses around the `<expr>` after the `->` can be omitted if it is only one word.

### Making lazy values

```
'wrap' (<expr>)
```

Makes a value lazily evaluated. The parentheses around the `<expr>` can be omitted if it is only one word.

### Using lazy values

```
'unwrap' (<expr>)
```

Evaluates a lazy value. The parentheses around the `<expr>` can be omitted if it is only one word.

## `data` directives

A `data` directive introduces a new type and one or more constructors into scope. It looks like this:

```
('data' <name> (<name> '::' <kind>)* '=' (<ctor> <type>*)+)
```

The parentheses around `<ctor>` can be omitted if no types follow it.

It introduces a new type of kind `fun <...> -> *`, where `<...>` is the kinds of the parameters, into scope. It also introduces one or more constructors, of type `fun <...> -> <name>` where `<...>` is the types of the constructor fields and `<name>` is the name of the data type, into scope.

## `let` directives

A `let` directive binds a term to a name. It looks like this:

```
('let' <name> (<name> '::' <kind>)* (<name> '::' <type>)* '::' <type> '=' <expr>)
```

It binds the name `<name>` to a function of zero or more parameters, evaluating to the thing on the right-hand side of the `=`. The type of the thing on the right-hand side of the `=` must evaluate to the `<type>` after the `::`. This is to simplify type inference.

## Notes

`let` and `data` directives can refer to names that were bound later in the file. So, order of directives does not matter.

