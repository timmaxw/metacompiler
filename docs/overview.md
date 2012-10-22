# `metacompiler`

`metacompiler` is an experimental compiler from SL, which is a simple Haskell-like language, to Javascript. It is unusual in that the compiler doesn't have enough built-in knowledge to translate SL into Javascript; instead, the user must provide a "translation" file that specifies how to translate different SL terms into Javascript expressions.

`metacompiler` takes two inputs: SL files, and translation files. The SL files define functions and types but do not specify how to express them as Javascript. The translation files specify ways to translate the functions and types from SL to Javascript. They also specify what should actually be included in the output. The output from `metacompiler` is a Javascript file which defines some functions or values.

## Table of contents

The documentation is broken up across the following files:

 *  `spec-language.md` defines the SL language.

 *  `translation-language.md` defines the syntax and semantics of the translation files, in detail.

 *  `tutorial.md` is an informal example-based explanation of how to write translation files.
 
 *  `inference.md` describes the algorithm for inferring translation-language terms from SL terms.

 *  `future-directions.md` summarizes some improvements that might be implemented at some point.

## Notational conventions

Both SL and the syntax of the translation files are based on S-expressions. The S-expressions used by `metacompiler` consist only of lists, atoms, and string literals. String literal escaping conventions are exactly as in Haskell. Comments begin with semicolons and run until the end of the line.

When describing the syntax that is built on top of the S-expressions, the following conventions will be used:

 *  Parentheses mean an actual list.

 *  `{` and `}` surround groups.

 *  A `?`, `*` or `+` after something means "repeat this (up to one / zero or more / one or more) times"

 *  Something enclosed in `<` and `>` means "something gets inserted here"

 *  `''` indicates a specific atom. This is so that atoms that contain `*`, `+`, and other meaningful characterss can be distinguished from those special characters themselves.

 *  `""` indicate a S-expression string.

For example,

```
('blah' '=' "<phone-number>" {',' "<phone-number>"}*)
```

means a list consisting of the atom `blah`, the atom `=`, and the one or more strings which represent phone numbers, separated by commas.
