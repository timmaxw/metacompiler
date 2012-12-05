1. ~~Translation-language parser~~

2. ~~Translation-language type-checker (deps: 1)~~

3. ~~Given translation-language term, compute JavaScript equivalent (deps: 2)~~

4. SL ~~parser~~, type-checker, and interpreter

5. ~~Front-end and `(emit ...)` (deps: 3)~~

6. Evaluate moving SL into `metacompiler`'s type system, and implement if appropriate (deps: 3)

7. Implement `use` and `infer` (deps: 2, 4)

8. Automatic verification (deps: 4)

