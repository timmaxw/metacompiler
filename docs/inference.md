# Inference

The goal of the inference algorithm is, given a SL term and a translation-language type, to build a translation-language term which has the gvien type and is equivalent to the given SL term, using only those terms that have been registered using `use` directives.

Here's a rough outline of the algorithm:

 1. Confirm that the term's type is the same as the type's SL equivalent.

 2. Look for registered translations that are equivalent to the SL type for some value of their parameters, assuming that the parameters can be inferred. In case of multiple matches, prefer the most specific one. If there are still multiple candidates, choose arbitrarily.

 3. Recursively infer on the sub-matches.

 4. If we get in a loop, fail. The only way we can loop is if there are registered translations whose SL equivalent is simply one of their parameters. In any sequence of such translations, no single translation should appear more than once.

