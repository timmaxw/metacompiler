module Metacompiler.JS.Eval where

import qualified Language.ECMAScript3.PrettyPrint as JS
import qualified Language.ECMAScript3.Syntax as JS
import System.Exit
import System.IO
import System.Process

-- This comes from the `System.IO.Strict` module, but that requires a separate
-- package, so I just copied the implementation for convenience.

hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

-- `evalJS` attempts to evaluate the given Javascript expression using node.js.
-- If it succeeds, it will return `Right x`, where `x` is a string
-- representation of the result. If it fails, it will return `Left x`, where
-- `x` is the backtrace from node.js.

evalJS :: JS.Expression a -> IO (Either String String)
evalJS expression = do
	(Just procStdin, Just procStdout, Just procStderr, procHandle) <-
		createProcess $ (proc "node" ["-p", "-e", JS.renderExpression expression]) {
			std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe
			}

	hClose procStdin

	hSetBinaryMode procStdout True
	hSetBinaryMode procStderr True

	stdoutContents <- hGetContentsStrict procStdout
	stderrContents <- hGetContentsStrict procStderr

	hClose procStdout
	hClose procStderr

	exitCode <- waitForProcess procHandle

	case exitCode of
		ExitSuccess -> return (Right stdoutContents)
		ExitFailure _ -> return (Left stderrContents)


