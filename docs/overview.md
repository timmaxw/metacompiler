# `metacompiler`

`metacompiler` is an experimental compiler from SL (a simple Haskell-like language) to Javascript. It is unusual in that the user can customize how different SL constructs are translated into Javascript. The customizations are described using a domain-specific language called TL.

## How `metacompiler` is used

The simplest way to use `metacompiler` is just to give it some SL source files, and no TL files. `metacompiler` will output JavaScript translations for the functions in the SL source files.

The JavaScript code produced by the above process may be inefficient. To improve the efficiency, the programmer can also provide one or more TL files as input to `metacompiler`. The TL files give specify a series of SL types or expressions, and their JavaScript equivalents. For example, they might specify that SL integers are to be represented as JavaScript numbers, and SL's `plus` function is to be expressed using JavaScript's `+` operator.

Often, a TL file will be included with a SL library, to provide JavaScript translations for the library functions. This way, the SL library's author can figure out the best way to represent the SL library in JavaScript once, and then all users of the library can benefit. However, TL files can also be distributed separately from SL files, to offer alternative implementations of existing SL programs; this is similar to how there are [drop-in](http://goog-perftools.sourceforge.net/doc/tcmalloc.html) [replacements](http://dmalloc.com/) available for C's default memory allocator.

## The SL language

SL is similar to a subset of Haskell, but a syntax based on S-expressions for ease of prototyping.

Here's a simple example of how to define Peano numbers in SL:

```
(data Nat = (Zero) (Succ Nat))

(let plus . (x :: Nat) (y :: Nat) :: Nat =
	(case x of
		(Zero .) -> y
		(Succ . x') -> (Succ . (plus . x' y))
	)
)

(let times . (x :: Nat) (y :: Nat) :: Nat =
	(case x of
		(Zero .) -> (Zero .)
		(Succ . x') -> (plus . y (times . x' y))
	)
)
```

For a detailed description of SL syntax, see the file `sl.md`.

## TL translation for `Nat`, `plus`, and `times`

Sorry, this section isn't written yet. Look at the `examples/` directory and `tl.md` for some idea of what's going on. This section will be written after the JavaScript inference mechanism is implemented, because it doesn't make sense before then.

## Current status, and future directions

Most of the features described above do not work; `metacompiler` is still in an early stage of development. See `milestones.md` for a description of what's missing from the features above. See `future-directions.md` for things that might be implemented at some later date.

