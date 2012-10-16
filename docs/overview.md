# `metacompiler`

`metacompiler` is an experimental compiler from SL, which is a simple Haskell-like language, to Javascript. It is unusual in that the compiler doesn't have enough built-in knowledge to translate SL into Javascript; instead, the user must provide a "translation" file that specifies how to translate different SL terms into Javascript expressions.

`metacompiler` takes two inputs: SL files, and translation files. The SL files define functions and types but do not specify how to express them as Javascript. The translation files specify ways to translate the functions and types from SL to Javascript. They also specify what should actually be included in the output. The output from `metacompiler` is a Javascript file which defines some functions or values.

The documentation is broken up across the following files:

 *  `spec-language.md` defines the SL language.

 *  `translation-language.md` defines the syntax and semantics of the translation files, in detail.

 *  `tutorial.md` is an informal example-based explanation of how to write translation files.


