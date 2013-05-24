build/metacompiler: Metacompiler/*.hs Metacompiler/*/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main -outputdir build/objs

build/run_all_tests: Metacompiler/*.hs Metacompiler/*/*.hs TestMetacompiler/*.hs
	ghc --make TestMetacompiler/AllTests.hs -o $@ -main-is TestMetacompiler.AllTests.main -outputdir build/objs

build/metacompiler-prof: Metacompiler/*.hs Metacompiler/*/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main -outputdir build/objs -prof -fprof-auto

run_all_tests: build/run_all_tests
	build/run_all_tests

clean:
	rm -f build/metacompiler build/run_all_tests
	rm -rf build/objs/*

run_examples: build/metacompiler examples/*.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc examples/NatAsNumberDemo.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc examples/FactorialRecursion.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc examples/FactorialOptimized.mc
	@echo ============================================================
	build/metacompiler examples/EitherAsPair.mc
	@echo ============================================================
	build/metacompiler examples/Function.mc
	@echo ============================================================
	build/metacompiler examples/Function2.mc
	@echo ============================================================
	build/metacompiler examples/MaybeAsNull.mc
	@echo ============================================================
	build/metacompiler examples/LazyAsFunction.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc examples/Function.mc examples/ListAsArray.mc
	@echo ============================================================
	build/metacompiler examples/NatAsNumber.mc examples/Function.mc examples/ListAsArray.mc examples/CountDownList.mc
	@echo ============================================================

