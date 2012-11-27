build/metacompiler: Metacompiler/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main

