# Fold

<img src="docs/img/hands.png" alt="drawing" width="260"/>

Fold is an experimental language and compiler for synthesis of digital circuits at a high level of abstraction. It is intended to be flexible and to support wide range of applications with predictable synthesis results.

Some features:

 * Users input high level description of the desired behavior in the form of a program. This program resembles C syntax in its form, supports variables of arbitrary bitwidth, and has arithmetic operators defined to never overflow.

 * Choice of architecture is up to the user and does not involve heuristics. Users annotate the program through special syntax and statements to influence the architecture.

 * The model for compilation of the program to logic generalizes both automatic pipelining and automatic compilation of finite state machines.

 * As a debugging aid, the programmatic description in Fold can be compiled into machine code to be executed as an ordinary computer program. The Fold toolchain provides certain guarantees about the matching behavior of the compiled machine code and the compiled logic.

 * Logic compiler integrates into [Yosys](https://github.com/YosysHQ/yosys). Produced netlist is in the Yosys' RTLIL format, can be exported to Verilog.

See the [Quick Start](docs/QuickStart.md) document.

## Setting up the compilers

### Logic compiler

The Fold logic compiler spans a frontend written in Python and a sequence of circuit processing passes written in C++. As a whole the compiler depends on Yosys with the Python binding (`ENABLE_PYOSYS := 1` build option of Yosys). To build the circuit processing passes, run `make` in the root of the repository. This will produce a Yosys plugin binary at `build/fold.so`. Yosys 0.35 at the minimum is required.

The logic compiler can be invoked with

	python -m fold.logic

in the root of the repository. It will automatically load the binary plugin. Pass `-h` to see help with command line usage.

### Machine code compiler

The machine code compiler is written purely in Python, depends on [llvmlite](https://github.com/numba/llvmlite), and is invoked with

	python -m fold.machinecode

## Sample flow

### Basic flow

To build a Fold program `sample.fold` into an RTLIL netlist `sample.il`:

	python -m fold.logic -o sample.il sample.fold

To simulate the program, supplying implementations for the `MUTEX_ASSERT` debugging cells that will be part of the netlist:

	yosys -p "read_rtlil sample.il; read_verilog -sv support/mutex_assert.sv; hierarchy -top top; proc; sim -clock clk -reset rst"

### Flow with full instrumentation

There's optional "ExecID" instrumentation that needs to be enabled for checking that the memory accesses in logic implementation [respect program order](docs/QuickStart.md#req-programorder). This "ExecID" instrumentation has the form of a circuit of cells with types which are prefixed with `EXECID_`. As of now these cells don't have an implementation apart from a very limited one. If you run into one of the limitations the cells trigger a failed assert.

Even this limited implementation serves to demonstrate the principle, and is a placeholder for a full-fledged implementation in the future.

To build a Fold program `sample.fold` into an RTLIL netlist `sample.il` including the ExecID instrumentation:

	python -m fold.logic --execid -o sample.il sample.fold

To simulate the program, supplying implementations for the `MUTEX_ASSERT` debugging cells that will be part of the netlist, and also supplying a limited implementation for the ExecID cells:

	yosys -p "read_rtlil sample.il; read_verilog -sv support/mutex_assert.sv support/dummy_execid.sv; hierarchy -top top; proc; sim -clock clk -reset rst"

## License

This work is distributed under the terms of the ISC license, see the copyright notice and the license text below.

```
Copyright 2023 Martin Povišer <povik@cutebit.org>

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
```

Commit messages include a `Signed-off-by` line to certify the contribution in the sense of the [Developer's Certificate of Origin 1.1](docs/DCO.txt)
