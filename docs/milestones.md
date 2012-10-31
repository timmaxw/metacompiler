1. Translation-language parser

2. SL parser, type-checker, and interpreter

3. Given translation-language term, compute SL equivalent (deps: 1, 2)

4. Type-check translation language (deps: 3)

5. Given translation-language term, compute Javascript equivalent (deps: 1)

6. Implement `use` and `infer` (deps: 3)

7. Put a front-end on it and implement `emit` (deps: 5)

8. Automatic verification (deps: 6, 7)

