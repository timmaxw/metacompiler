build/metacompiler: Metacompiler/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main -outputdir build/objs

test: Metacompiler/*.hs Test/*.hs
	ghc --make Test/Runtime.hs -o build/test -main-is Test.Runtime.main -outputdir build/objs
	build/test

