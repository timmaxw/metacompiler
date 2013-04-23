# S-expressions

Both SL and TL use S-expressions for their syntax. This is a specification for those S-expressions.

The S-expressions used by `metacompiler` consist only of lists, atoms, and string literals.

Lists are, of course, delimited by `(` and `)`.

Atoms may contain any printable non-whitespace character except for `"()[];`.

There are two kinds of string literals. One kind is delimited by `"` and supports the string escapes `\n`, `\r`, `\t`, `\'`, `\"`, and `\\`. The other kind begins with a `[`, followed by zero or more `=`s, then a `[`, and ends with `]`, followed by the same number of `=` that started it, followed by `]`. It can span multiple lines. There are no escape sequences; everything is interpreted literally except for the end-delimiter.

Comments begin with semicolons and run until the end of the line.

Carriage-return (`\r`) and tab (`\t`) characters are illegal except within `[[...]]`-style string literals.
