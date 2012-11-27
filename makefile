build/metacompiler: Metacompiler/*.hs
	ghc --make Metacompiler/Main.hs -o $@

