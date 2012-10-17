# Future directions

 *  Automatic verification of `js-expr` terms. When `metacompiler` is run in verification mode, it ignores `emit` directives and instead generates a Javascript function that tests every `js-expr` term and displays an appropriate error message if the `impl` clause of some `js-expr` does not have the same semantics as its `spec` clause. It does this by generating plausible values for the inputs of the `js-expr` using a SL interpreter to compute the expected outputs. Then it uses the inference algorithm to generate Javascript representations of both inputs and outputs, and then generates code to evaluate the `js-expr`'s Javascript with the inputs and then compare for equality with the outputs. `metacompiler` would need to get special knowledge of equality and Booleans.

 *  Additional verification: Add an optional `verifier` clause to `js-repr` directives. This would be a piece of Javascript code to run on a value that's supposedly represented using this representation. The code should throw an exception if the value is invalid. For example, if SL integers are being represented as Javascript numbers, the verifier can assert that `typeof input == "number"`. When `metacompiler` is run in verification mode, it can insert a call to the verifier after each chunk of user-supplied Javascript code.

 *  Right now, all terms are represented as Javascript expressions which can be evaluated arbitrarily many times and have no side effects. If `metacompiler` allowed terms to destroy their arguments as long as they indicated that they were doing so, then `metacompiler` could generate imperative code. This would require making meta-types more complicated, and there are some subtle issues involving how to represent object ownership.

 *  Sometimes the simple pattern-matching approach to inferring translations isn't enough. For example, it would be nice to have integer literals in SL that can be translated automatically to Javascript integer literals. Right now, that's impossible without manually adding a translation for each integer. The same goes for strings. It might be possible to extend the translation system to allow people to write their own translators. Alternatively, it might be reasonable to special-case integers and strings.

 *  `metacompiler` should have something resembling a module system. SL should have a standard library, and there should be a set of translations to go along with the standard library.

 *  SL could be nicer.

 *  `metacompiler` could target languages other than Javascript.

