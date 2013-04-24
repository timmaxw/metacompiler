# S-expressions

Both SL and TL use S-expressions for their syntax.

## Specification of `metacompiler`'s S-expressions

The S-expressions used by `metacompiler` consist only of lists, atoms, and string literals.

Lists are, of course, delimited by `(` and `)`.

Atoms may contain any printable non-whitespace character except for `"()[];`.

There are two kinds of string literals. One kind is delimited by `"` and supports the string escapes `\n`, `\r`, `\t`, `\'`, `\"`, and `\\`. The other kind begins with a `[`, followed by zero or more `=`s, then a `[`, and ends with `]`, followed by the same number of `=` that started it, followed by `]`. It can span multiple lines. There are no escape sequences; everything is interpreted literally except for the end-delimiter.

Comments begin with semicolons and run until the end of the line.

Carriage-return (`\r`) and tab (`\t`) characters are illegal except within `[[...]]`-style string literals.

## Notational conventions for S-expression-based syntaxes

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
