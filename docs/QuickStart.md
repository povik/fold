# Quick Start for Fold Users

Fold is an experimental language and compiler for synthesis of digital circuits at a high level of abstraction. It is intended to be flexible and to support wide range of applications with predictable synthesis results.

A design written in Fold takes the form of a program which combines procedural and functional styles of programming. This level of description is supplemented with "folding hints" and special statements which instruct the compiler on architectural choices in implementing the program in digital circuitry. The program that is input to the Fold compiler is composed of conventional control flow elements: if-else statements, for loops, function definitions and function calls. This is combined with variable declarations with variables which are either *immutable*, whose value can only be assigned once, or *mutable*, whose value can be updated in a sequence of assignments. Fold programs use conventional arithmetic operators which are defined to never overflow. Wraparound of integer values only occurs once values are assigned to variables since variables have defined and limited bit widths chosen by the user.

For a reference on the language in which the programs are written, see the language reference (TODO).

Fold programs can be compiled down to performant machine code to test out the programmatic description while ignoring any choice of architecture for the digital logic. See the machine code compiler (TODO).

## Sample program (RISC-V core)

We are going to build a minimal pipelined RISC-V core from scratch. On the way we are going to discuss various Fold features as they come into relevance. We will not be striving for perfect compliance with the RISC-V specification.

As a first step we will write a Fold program that can be compiled with the machine code compiler, and only once that's done will we concern ourselves with annotating the program with "folding hints" and making other changes to the program so that it can be compiled to digital logic.

Let's start by defining a bunch of constants, these are integer values taken from the RISC-V standard, and we are going to give them names so we can refer to them later.

```
# opcode constants
const LUI    = 0x0d << 2 | 3;
const AUIPC  = 0x05 << 2 | 3;
const JAL    = 0x1b << 2 | 3;
const JALR   = 0x19 << 2 | 3;
const BRANCH = 0x18 << 2 | 3;
const LOAD   = 0x00 << 2 | 3;
const STORE  = 0x08 << 2 | 3;
const OP_IMM = 0x04 << 2 | 3;
const OP     = 0x0c << 2 | 3;
const MEM_MISC = 0x03 << 2 | 3;
const SYSTEM   = 0x1c << 2 | 3;

# funct3 constants
const BEQ  = 0;
const BNE  = 1;
const BLT  = 4;
const BGE  = 5;
const BLTU = 6;
const BGEU = 7;
const LB  = 0;
const LH  = 1;
const LW  = 2;
const LBU = 4;
const LHU = 5;
const SB = 0;
const SH = 1;
const SW = 2;
```

Next, we define a variable for the working memory of the processor.

```
var wm [128, 4, 8] mut;
```

We made the variable three-dimensional. The first dimension corresponds to the words, the second dimension to the bytes in a word, and the last dimension to the bits of a byte. The variable is named `wm` and we are signifying, with the `mut` keyword, that it's mutable. If it wasn't, each memory cell could only ever be assigned a single value for the duration of the program's execution. (If the variable was defined at other than the top scope, the immutability restriction would apply for execution *within that scope*, not for the entirety of the program's execution.)

Now we write a function to access the working memory. We will have a single function to carry out both reads and writes. We are writing it this way because it will help with mapping to hardware primitives later on once we are compiling to digital logic.

```
func memio(addr [32], wdata [32], be [4], we [1]) (rdata [32])
{
	if (we) {
		if be[0] { wm[addr >> 2, 0] = wdata[7..0]; }
		if be[1] { wm[addr >> 2, 1] = wdata[15..8]; }
		if be[2] { wm[addr >> 2, 2] = wdata[23..16]; }
		if be[3] { wm[addr >> 2, 3] = wdata[31..24]; }
	} else {
		rdata = (wm[addr >> 2, 0] | wm[addr >> 2, 1] << 8 |
					 wm[addr >> 2, 2] << 16 | wm[addr >> 2, 3] << 24);
	}
}
```

In the function, we can access the `wm` variable from above since it was declared at the top scope, so the variable is visible within the function (and any other function). The function has a single return value represented by the variable `rdata` to which we assign in the function body if we are doing reads (argument `we` being zero). Function's arguments and return values are both held in implicit immutable variables tied to the function's scope.

Now let's write the main loop interpreting the RISC-V instructions. First we define the register file (`regs`) and program counter (`pc`), then in a loop we fetch the instruction at the memory position pointed to by the program counter, and we decode some fields out of the instruction.

```
var regs [16, 32] mut;
regs[0] = 0;
var pc [32] mut;
pc = 0;
for 1 {
	var i [32], opcode [7], pc_sample [32];
	i = memio(pc, undef, undef, 0);
	pc_sample = pc;   
	pc = pc_sample + 4;

	var opcode [7];
	opcode = i[6..0];

	print("[%08x] %08x\n", pc_sample, i);

	var funct3 [3], funct7 [4], rs1 [5], rs2 [5], rd [5];
	funct7 = i[31..25]; rs2 = i[24..20]; rs1 = i[19..15]; funct3 = i[14..12]; rd = i[11..7];

	var imm_i [12] signed, imm_s [12] signed, imm_b [13] signed, imm_u [32] signed, imm_j [21] signed;
	imm_i = i[31..20];
	imm_s = i[31..25] << 5 | i[11..7];
	imm_b = i[31] << 12 | i[7] << 11 | i[30..25] << 5 | i[11..8] << 1;
	imm_u = i[31..12] << 12;
	imm_j = i[31] << 20 | i[19..12] << 12 | i[20] << 11 | i[30..21] << 1;

	# TODO: fetch operands from registers
	# TODO: carry out operation, write result to register
}

```

Here there's only a few noteworthy points. We are passing `undef` as a value for the second and third argument of `memio`. This represents an undefined value, which we are free to pass in since for reads those arguments are ignored, and passing in undefined values can be an opportunity for the logic compiler to better optimize the resulting netlist. For a reference on the instruction fields we are decoding in the code here, refer to Figure 2.3 of the RISC-V [Unpriviliged Specification](https://github.com/riscv/riscv-isa-manual/releases/download/Ratified-IMAFDQC/riscv-spec-20191213.pdf), page 16.

Notice that we have a `print` statement in the code printing out the program counter and the instruction we fetched out of working memory. This will be preserved through to the netlist once we are compiling to digital logic (it will be expressed through special `$print` cell of RTLIL).

We have fetched and decoded the instruction. Now we want to source the operands from the register file. We write the following code:

```
	var r1 [32], r2 [32], r1s [32] signed, r2s [32] signed;
	r1 = regs[rs1];
	r2 = (opcode != OP_IMM) ? regs[rs2] : imm_i;
	r1s = r1; r2s = r2;
```

We read the registers indexed by the `rs1` and `rs2` fields of the instruction to obtain the first and the second data operand respectively. We special-case the `OP_IMM` opcode and take the second operand directly from the `imm_i` field instead. We are assigning the same operand data to `r1s` and `r2s` for reinterpretation. Assignment to variables and passing in of function arguments are the only basic operations in Fold programs which can overflow. The overflow semantics are defined, and here we make use of them to reinterpret the same data as signed integers. From now on whenever we wish to operate with the operands, we can use `r1` and `r2` as their unsigned integer interpretation, and `r2s` and `r2s` as their signed interpretation.

Next we define a helper `wb` function to write a result of an operation back to the register file.

```
	func wb(r [32]) () {
		if rd != 0 { regs[rd] = r; }
	}
```

This is a function declared within the scope of the loop body, and as such it sees the `rd` variable holding the index of the target register even if that's not explicitly passed in. The function performs the check for the `rd == 0` case which the specification says we should ignore as a register write.

We move on to the execution of the actual operation encoded in an instruction, we implement couple of opcodes:

```
	if opcode == LOAD {
		var addr [32], mem_r [32], r [32];
		addr = r1 + imm_i;
		mem_r = memio(addr & ~3, undef, undef, 0) >> 8 * addr[1..0];
		if funct3 == LW  { r = mem_r; }
		if funct3 == LH  { var h [16] signed; h = mem_r; r = h; }
		if funct3 == LHU { var h [16]; h = mem_r; r = h; }
		if funct3 == LB  { var h [8] signed; h = mem_r; r = h; }
		if funct3 == LBU { var h [8]; h = mem_r; r = h; }
		wb(r); # load write-back
	} else if opcode == STORE {
		var addr [32], mask [4];
		addr = r1 + imm_s;
		if funct3 == SB { mask = 1; }
		if funct3 == SH { mask = 3; }
		if funct3 == SW { mask = 15; }
		_ = memio(addr & ~3, r2 << (8 * addr[1..0]), mask << addr[1..0], 1);
	} else if opcode == JAL {
		pc = imm_j + pc_sample;
		wb(pc_sample + 4);
	} else if opcode == JALR {
		pc = (r1 + imm_i) & -2;
		wb(pc_sample + 4);
	} else if opcode == OP || opcode == OP_IMM {
		var r [32];
		if !(opcode & 0x20) || !(funct7 & ~0x20) {
			if funct3 == 0 { r = (opcode & 0x20) && i[30] ? r1 - r2 : r1 + r2; } # addi/add/sub
			if funct3 == 2 { r = r1s < r2s; } # slti/slt
			if funct3 == 3 { r = r1 < r2; } # sltiu/sltu
			if funct3 == 4 { r = r1 ^ r2; } # xori/xor
			if funct3 == 6 { r = r1 | r2; } # ori/or
			if funct3 == 7 { r = r1 & r2; } # andi/and
		}
		var shamt [5];
		shamt = r2;
		if funct3 == 1  { r = r1 << shamt; } # slli/sll
		if funct3 == 5 && !i[30] { r = r1 >> shamt; } # srli/srl
		if funct3 == 5 && i[30]  { r = r1s >> shamt; } # srai/sra
		wb(r);
	} else if opcode == LUI {
		wb(imm_u);
	} else if opcode == AUIPC {
		wb(imm_u + pc_sample);
	} else if opcode == BRANCH {
		var cond [1];
		if funct3 == BEQ  { cond = r1 == r2; }
		if funct3 == BNE  { cond = r1 != r2; }
		if funct3 == BLT  { cond = r1s < r2s; }
		if funct3 == BLTU { cond = r1 < r2; }
		if funct3 == BGE  { cond = r1s >= r2s; }
		if funct3 == BGEU { cond = r1 >= r2; }
		if cond { pc = pc_sample + imm_b; }
	}
```

Here we mechanically follow what the specification says. To simplify things for us a bit, we don't distinguish between instructions we are free to ignore (e.g. `FENCE`) and invalid instructions. We also assume all memory accesses are aligned to a boundary of the data unit they are accessing (bytes, halfwords, words).

If we put all the pieces together and compile the code with the machine code compiler, we obtain a minimal RISC-V instruction emulator supporting the unpriviliged RV32E ISA. We are only missing to initialize the working memory with an actual image of a RISC-V program. We can do that at the top of the Fold program with

```
wm = read_tsv!('program_image.tsv');
```

provided `program_image.tsv` contains the program image in a suitable TSV format. (TODO: link to what the format is)

### Compiling the sample program to logic

There's plenty to cover when compiling programs to logic. At first we will try to minimally annotate our program to make it synthesizable, then we will make a few adjustments to improve the clock cycle efficiency of our core. First let's cover some general background of Fold.

Fold programs are compiled to synchronous logic with global clock and reset domains. In the following we will call the details of the scheduling and other choices one must make in implementing the program in digital logic the *architecture*. This architecture will to some degree be influenced by the user through special annotations and program statements. It's not the case that any arbitrary Fold program can be successfully compiled to logic, rather the user of Fold needs to understand the result the compiler will be synthesizing, and carries the burden to make sure certain requirements on the program and the architecture are met. Fold tooling will help by performing checks for the requirements partly at synthesis time and partly at simulation time.

The requirements for a correct implementation of the program in logic are of three kinds:

  1. **Data causality:** In an implementation, the primitive operations that make up the program are scheduled with respect to clock cycles, and this scheduling needs to respect data causality, that is, a result of an operation needs to be available at the latest in the same cycle that the result is used for an input of another operation. The details of scheduling of operations are left to the compiler, which makes sure data is appropriately forwarded, but user instructions related to architecture can make scheduling insolvable. In that case the user instructions are deemed in conflict with data causality. Data causality is a requirement for synthesis, and is fully checked at synthesis time.

  2. **Conflict-free usage of resources:** Implementations have exclusive resources that can only be used once each clock cycle, but depending on the architecture and the course of the program's execution, a usage conflict can arise with the exclusive resource being requested for more than one job. These usage conflicts cannot be ruled out by the compiler at synthesis time, instead the compiler produces assertions that are part of the output netlist to detect conflicts at simulation time.

  3. **Respecting of program order in memory accesses**: Users can request for a variable in the Fold program to be implemented with a block memory in the output netlist. In those cases the user influences the timing of the accesses to the memory through architectural annotations, and can reorder the accesses from the order they appear in the program. For a faithful implementation of the program, we need to make sure that for each memory read, the last write to the memory cell being read is the write that's supposed to be last according to the program order. This is another requirement for which the compiler produces assertions in the output netlist.

One way users annotate the program to influence the architecture is by inserting `delay(N);` statements. These `delay(N);` statements have the semantics of delaying the execution of the program by the given number of virtual clock cycles. This defines a *virtual* timing of how the operations of the program are supposed to be scheduled in the logic implementation. This virtual timing is adjusted by the compiler to solve for detailed data causality, and to minimize the amount of registers required, but some architectural properties are kept over from, and defined by, the virtual timing. For example the virtual cycle offset from one execution of a statement to another execution of the same statement, e.g. in a loop body, will match the physical clock cycle offset of the respective activations of the implementing circuitry. This applies generally to any paths of execution from a statement to itself as long as the statement has a singular implementation, which would usually be the case.

For each statement in the Fold program the circuitry implementing the statement counts among the exclusive resources the usage of which must be kept conflict-free. For the avoidance of conflicts the virtual clock cycle offset as we trace a viable execution of the program from a statement to itself cannot be zero.

Returning to our RISC-V core, we have a loop and the function calls to `memio` to consider in avoiding conflicts. The statements in the `memio` function body are a possible source of conflicts depending on the virtual timing pattern of the `memio` calls. Let's focus on the loop body at first. We have three different occurences of a call to `memio`, but based on viable paths of execution, we only need to introduce a virtual delay between the first call and the second, and the first call and the third. To that end we can put `delay(1);` just below the first `memio` call. Then we need to consider paths looping back as another source of conflicts, we can address those by appending a second `delay(1);` statement at the end of the loop body.

Schematically we now have:

```
var regs [16, 32] mut;
regs[0] = 0;
var pc [32] mut;
pc = 0;
for 1 {
	var i [32], opcode [7], pc_sample [32];
	i = memio(pc, undef, undef, 0);
	delay(1);
	pc_sample = pc;   
	pc = pc_sample + 4;

	# instruction decoding
	...

	# fetching of operands from registers
	...

	# instruction execution (involves calls to memio!)
	...

	delay(1);
}
```

At this point we should have a synthesizable and correctly implemented Fold program, but since we haven't asked for the `wm` variable to be implemented with a block memory, it will be implemented directly with flip-flops in the output netlist and involve an excessive amount of logic.

#### Block memories

To opt for a block memory backing for the `wm` variable, and also for the `regs` variable, we need to annotate both declarations. We do that in the following way:

```
var wm [128, 4, 8] mut; ` mem.
var regs [16,32]; `mem.
```

These architectural annotations appended after the statement are called "folding hints" and are found in a special syntax resembling code comments: starting with the backtick character `` ` `` and ending with a newline. The individual folding hints are separated with a period character. The hint spelled `mem` requests a variable to be implemented in block memory.

Turning a variable into a memory-backed one means that we, as the user, are in control of the physical timing of the accesses and are responsible for making sure the accesses respect the program order. We control the timing because the memory accesses are defined to occur in the timing pattern that matches the virtual timing of the statements. (To be precise: The timing is defined to match the virtual timing *up to a constant offset*, and this applies to each memory-backed variable in isolation. Reads or writes to two distinct memory-backed variables need not be implemented to occur in the same cycle even if in virtual timing they do.)

Block memories implementing variables have synchronous read and write ports. By default those read ports are non-transparent, which means they are defined not to see data updates from writes carried out within the same cycle, but with a folding hint they can be made transparent. This is factored in when deciding whether memory accesses respect program order.

Separately from the issue of memory accesses respecting program order, the timing of memory reads and writes is a key factor for the program to meet data causality. The user explicitly schedules the memory reads and writes, and this can easily make for a constraint violating data causality. We can demonstrate that on our core. Consider the following sequence of operations in our program when it's processing a `LOAD` or `STORE` instruction:

 1. the `wm` read to obtain the instruction

 2. reading from `regs` at the index `rs1` decoded out of the instruction

 3. the `wm` data access at an address derived from the `regs[rs1]` value

Both steps 2 and 3 depend on data from the previous steps. In our program, step 3 is carried out one virtual cycle after step 1, and because in both steps we are accessing the same memory-backed variable, this will fix the physical timing of the memory accesses: the memory access in step 3 is carried out one physical clock cycle after the memory access in step 1. This is in conflict with the availability of the data that is used for the address in memory access 3. The address is derived from the result of operation 2, which itself can only be carried out once the result of operation 1 is available. Because those are memory reads taking one cycle to produce their result, operation 3 can only be carried out *two cycles* past operation 1 at the earliest, otherwise we don't meet data causality.

We can resolve the conflict by inserting a `delay(1);` statement just after the fetching of operands from registers, and thus postponing step 3 by one virtual cycle.

Even then, in our core as it stands, there's still yet another data causality violation. Let's say we are processing a `LOAD` instruction. We continue the steps from above with step 4:

 4. write to `regs[rd]` with data fetched in step 3

As we trace execution from step 2 to step 4, we are delayed by one virtual cycle with the `delay(1);` statement we have just added. Because in steps 2 and 4 we are once again accessing an identical memory-backed variable, this time it's `regs`, we are constraining the physical timing: The memory write in step 4 ought to be performed in the next cycle after the memory read in step 2. As an important detail, for both memory reads and writes, Fold considers memory accesses carried out in the cycle in which the address is presented on the memory port, so for a write this would the same cycle in which the data to be written is presented, and for reads this would be one cycle ahead of the read data being produced back. So considering the memory accesses 2, 3, 4 in our core, with similar reasoning as before, we find out step 4 can only be carried out *two cycles* past step 2 for data causality sake. To resolve the conflict here, we can add another `delay(1);` statement just before the write to the register file in handling of the `LOAD` opcode.

Once we have added those new `delay(1);` statements, we can remove the one final `delay(1);` statement at the end of the loop body that we introduced to avoid conflicts with execution paths looping back, since this one is no longer required.

There's one final detail to tweak. We are initializing the core's working memory with

```
wm = read_tsv!('program_image.tsv');
```

and there are two issues with this. First, the `memio` call to fetch the first instruction won't see this write, since that's in the same virtual cycle, and second, this will be compiled into a very wide memory write port in the netlist. To instead emit memory initialization data in the netlist, say, for an FPGA primitive, we can annotate the statement:

```
` meminit.
wm = read_tsv!('program_image.tsv');
```

Once we have made those changes to insert the virtual delays and after we have annotated the `wm` initialization, the program once again becomes synthesizable and is faithfully implemented. This time we have made the variables `wm` and `regs` memory-backed.

#### Pipelining

In the code we have written so far we have three `delay(1);` statements in the loop body, two of which are executed unconditionally and one only in the case of the `LOAD` opcode. Even though those are virtual delays, and the details of the scheduling of operations in the program are left to the compiler, we can reason using the principles we have already introduced to understand the number of clock cycles it takes for the synthesized core to process one instruction.

We can pick any top-level statement in the loop body, and picture a path of execution from the statement to itself. This path will pick up a virtual delay of two or three cycles, and by the principle we have stated earlier this will also be the physical clock cycle period of the activation of the circuitry implementing the statement. By this we know it takes two or three physical clock cycles for the core as-is to process an instruction, though we don't know in detail what stage of processing will happen in what cycle. A priori we don't even know if the core will be done with one instruction before it starts processing the next (as a matter of fact it won't, as the write to the register file from processing the preceding instruction will overlap with fetching the next instruction from working memory).

As the next improvement to our core, we wish for the core to process one instruction per clock cycle, except for `LOAD` and `STORE` instructions accessing the working memory and except for jumps and branching. We can do that with careful adjustement of the virtual timing, and a small change to the control flow of the program. This will in effect make our core pipelined: The synthesized core will be processing multiple instructions at the same time in different stages of processing (even more so than it does already).

First let us comment on the `memio` function and the usage constraints inherent in it. We have said earlier we wrote the program of the core to use the `memio` funtion for all working memory accesses to help with mapping to hardware primitives later on. This is because we wish to infer a memory with a single R/W port, capable of at most a single read or single write each cycle. Having a `memio` function for all accesses (which as an ordinary function can only be invoked once each virtual cycle) makes the restriction of one R/W operation per cycle manifest in the synthesized circuitry, aiding in inference of the right kind of memory port downstream in the synthesis flow once the netlist compiled by Fold is being mapped to available hardware primitives.

The one R/W port limitation brings us to why the `LOAD` and `STORE` instructions must be an exception to the one-instruction-per-cycle processing rate: Those instructions will require two accesses to the working memory to process, but we can only have one access each cycle. So when processing a `LOAD` or `STORE` instruction we need to postpone some later instruction fetches, and we will one way or another temporarily lower the rate of instruction processing.

After we fetch an instruction, we need two cycles before we can be ready with the `LOAD` or `STORE` data access to the working memory because the address of the access is derived from a register value. For efficiency, we can interleave the fetch of the next instruction with the processing of the `LOAD` or `STORE` instruction: The next instruction will be fetched the next cycle, but the rest of the processing of this next instruction, and processing of all future instructions, will be postponed with an extra delay of one cycle.

So let's say we are processing instructions 1, 2, 3, 4 one after another, and label `Ix` the instruction fetch for instruction `x`, and `Dx` the data access for a `LOAD` or `STORE` instruction `x`.

If instruction 1 is a `LOAD` or `STORE`, we are looking at the following sequence of accesses to working memory:

| `I1` | `I2` | `D1` | `I3` | `I4` | ... |
| --- | --- | --- | --- | --- | --- |

If also instruction 2 is a `LOAD` or `STORE`, the sequence would be:

| `I1` | `I2` | `D1` | `I3` | `D2` | `I4` | ... |
| --- | --- | --- | --- | --- | --- | --- |

With this picture for the sequencing of accesses to the working memory, we can start implementing the code changes to our core. Let's make a list of the changes we need to make:

 * Overall, if our goal is to process an instruction each cycle (up to some exceptions), we need to make the virtual delay of an execution through the loop's body be one cycle. We can't altogether remove the delays we inserted earlier for data causality, but we can add a negative delay at the end of the loop iteration, and an extra one in handling of the `LOAD` opcode, such that the delay total comes out to one. This will be in conflict with data causality for the `pc` variable on jumps or branches, but we will address that separately.

 * To implement a conflict-free sequencing of working memory accesses according to the scheme we discussed, we introduce a new mutable variable `extra_delay1` to signal from the processing of a `LOAD` or `STORE` instruction that the processing of the next instruction should have an extra virtual delay inbetween the fetching of the instruction and the rest of the processing.

 * Let's focus on the `regs` accesses and discuss the timing of the reads that follow after the write of a result to `regs[rd]` in the processing of an instruction. By the insertion of the negative delays, we have made the `regs` read in the follow-up instruction be carried out one or two physical cycles earlier that they would be otherwise, in some cases reordering the read *before* the write (as long as the port in non-transparent). With this state of affairs we are failing to uphold the program order. Luckily making the read ports transparent is all we need to do. Even in the processing of a `LOAD` instruction, which causes a late register write, this will work out because of the extra delay signaled into the next instruction by `extra_delay1`.

 * To address data causality around `pc`, we need to insert virtual delays in case of jumps or branching instructions. `pc` is the fetch address for the next instruction, so the fetching of the next instruction needs to be appropriately delayed from the fetching of the current instruction so that there's a feasible scheduling of the operations that depend on the current instruction and influence the `pc` for the next instruction. We add a `delay(2);` statement to code blocks of opcodes `JAL`, `JALR`, and `BRANCH` each.

This is what our code looks like now, with some parts redacted out:

```
...

var wm [128, 4, 8] mut; ` mem.
` meminit.
wm = read_tsv!('program_image.tsv');

func memio(addr [32], wdata [32], be [4], we [1]) (rdata [32])
{
	...
}

var regs [16, 32] mut; ` mem.
regs[0] = 0;
var pc [32] mut;
pc = 0;
var extra_delay1 [1] mut;
extra_delay1 = 0;
for 1 {
	... (contains memio call)

	if extra_delay1 == 1 {
		delay(1);
	}
	extra_delay1 = 0;

	var r1 [32], r2 [32], r1s [32] signed, r2s [32] signed;
	` transp.
	r1 = regs[rs1];
	` transp.
	r2 = (opcode != OP_IMM) ? regs[rs2] : imm_i;
	r1s = r1; r2s = r2;
	delay(1);

	func wb(r [32]) () {
		if rd != 0 { regs[rd] = r; }
	}

	if opcode == LOAD {
		... (contains memio call)
		delay(1);
		wb(r);
		delay(-1);
		extra_delay1 = 1;
	} else if opcode == STORE {
		... (contains memio call)
		extra_delay1 = 1;
	} else if opcode == JAL {
		pc = imm_j + pc_sample;
		wb(pc_sample + 4);
		delay(2);
	} else if opcode == JALR {
		pc = (r1 + imm_i) & -2;
		wb(pc_sample + 4);
		delay(2);
	} else if opcode == OP || opcode == OP_IMM {
		...
		wb(r);
	} else if opcode == LUI {
		wb(imm_u);
	} else if opcode == AUIPC {
		wb(imm_u + pc_sample);
	} else if opcode == BRANCH {
		...
		if cond { pc = pc_sample + imm_b; }
		delay(2);
	}
	delay(-1);
}
```

The code of our program in full is here (TODO). It properly synthesizes into a pipelined RISC-V core. All the while the code can still be compiled with the machine code compiler. Note that the block we have added for proper sequencing, which was

```
	if extra_delay1 == 1 {
		delay(1);
	}
```

can be optimized out by the machine code compiler since `delay(N);` is a no-op when compiling to machine code. By extension the compiler can optimize out all of the variable `extra_delay1`.

#### Branch prediction

We can implement trivial branch prediction and speculative execution into our core! We will predict all branches not to be taken, that is, when we encounter a `BRANCH` instruction, we will first go on as if the execution wasn't redirected, and only a couple of cycles later once the result of the branch conditional is available will we correct our prediction if need be.

This would be cumbersome to express in the Fold program if it wasn't for the `fork` keyword, allowing us to fork the execution of the Fold program into multiple threads. The program execution semantics, program order considerations, and the need to avoid conflicting usage of resources carries over to when the execution of the Fold program is split into two or more threads. The `fork` keyword in the language is accompanied with a `quit` keyword to terminate a thread of execution.

Let's put the whole of our instruction processing loop beneath a code block, so that we can scope some variables to it, attach a `restart` label to the block, and introduce a `start_pc` variable for the starting value of `pc`:

```
var start_pc [32] mut;
start_pc = 0;

restart:
{
	var pc [32] mut;
	pc = start_pc;
	var extra_delay1 [1] mut;
	extra_delay1 = 0;
	for 1 {
		...
	}
}
```

In the processing of a `BRANCH` instruction, we won't modify the `pc` variable, instead if we figure out the branch is to be taken, we cancel the thread of execution that proceeded as if the branch wasn't taken (we use a new special variable to signal the cancellation), and instead fork out to a new thread of execution in which the branch is taken. We write the following:

```
	if opcode == BRANCH {
		var cond [1];
		... resolve cond

		if cond {
			fork skip;
			delay(2);
			start_pc = pc_sample + imm_b;
			goto restart;
		skip:
			flush = 1;
		}
	}
```

Forking with one thread of execution being already cancelled is a quirky way to write the program from the perspective of being compiled to machine code, but in digital logic implementation we are making use of the fact that the cancellation can occur some cycles later, meaning we are causally not as constrained on the availability of the `cond` value.

When exactly should the obsoleted thread of execution be cancelled when `cond` turns out to evaluate to 1? We express that through what we do with the `flush` variable: in the instruction processing loop, we keep historical values of `flush` from one and two cycles ago: `flush1` and `flush2`. And then finally if `flush2` contains 1, we quit the thread of execution just after the fetching of operands from the register file. Schematically:

```
...
var start_pc [32] mut;
start_pc = 0;

restart:
{
	var pc [32] mut;
	pc = start_pc;
	var extra_delay1 [1] mut;
	extra_delay1 = 0;

	var flush1 [1] mut, flush2 [1] mut;
	flush1 = 0;
	flush2 = 0;

	for 1 {
		var flush [1] mut;
		flush = 0;

		... fetch instructions

		... fetch operands

		if flush2 {
			quit;
		}

		... execute instruction

		flush2 = flush1; flush1 = flush;
		delay(-1);
	}
}
```

Additionally, we need to disable side-effects when `flush1` equals 1: both writes to `wm` and writes to `regs` shouldn't be carried out when `flush1 == 1`. Since we are using a memory for these variables, the writes in the cancelled thread would spill over to the main thread, which would be in violation of the requirement for memory accesses to respect the program order and would cause our core to misbehave. Once we add those conditionals, that's it! Full code here (TODO).
