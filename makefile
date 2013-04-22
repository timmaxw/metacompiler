build/metacompiler: Metacompiler/*.hs Metacompiler/*/*.hs
	ghc --make Metacompiler/Main.hs -o $@ -main-is Metacompiler.Main.main -outputdir build/objs

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

