module TestMetacompiler.AllTests where

import TestMetacompiler.TLRuntime
import Test.HUnit

main = runTestTT allTests >> return ()

