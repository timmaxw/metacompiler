`metacompiler` is an experimental compiler from a Haskell-like language to JavaScript that allows a human to assist the compiler in efficient compilation. It's basically a research project and is not meant for real use. Tim Maxwell is writing `metacompiler` at Caltech under the supervision of Mike Vanier for a CS 81: Undergraduate Projects in Computer Science class.

## Dependencies

* Haskell

* Language.ECMAScript3, to parse and manipulate JavaScript. With Cabal: `cabal install language-ecmascript`

* Node.js (<http://nodejs.org/>), to evaluate the JavaScript results. With `apt-get`: `sudo apt-get install nodejs`

