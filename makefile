build/metacompiler: Metacompiler/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main -outputdir build/objs

run_tests: Metacompiler/*.hs Test/*.hs
	ghc --make Test/Runtime.hs -o build/run_tests -main-is Test.Runtime.main -outputdir build/objs
	build/run_tests

