# Machine code compiler

The machine code compiler is invoked with

	python -m fold.machinecode

in the root of the repository. It compiles Fold programs to LLVM IR, and if invoked with the `--jit-exec` or `-e` option, it passes the IR to a JIT compiler targeting native machine code and immediately executes the program.

The machine code compiler ignores all architectural hints aimed at compilation to logic, but interprets a few hints of its own.

## Forks

The `fork` keyword is implemented with system-level process forks, which in some cases can lead to excessive number of processes and exhaustion of resource limits. In some use cases one can write

	` mcode_fork take_jump.
	fork target;

to signal to the machine code compiler it should ignore the thread of execution below the `fork` statement. Effectively the hint turns the `fork` into a `goto` as far as the machine code compiler is concerned. The hint spares the system-level process fork if the user knows the behavior of the program is unaffected.

Similarly, one writes

	` mcode_fork ignore_jump.
	fork target;

to simply ignore the `fork` statement by the machine code compiler, and proceed with execution of the statements below it.
