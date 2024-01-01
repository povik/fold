#!/bin/bash
# Use this to check the code samples in QuickStart.md do work as promised

set -ex

block () {
	awk -v no=$1 'BEGIN{p=1+2*no;} /```/{p-=1;flag=(p == 0);next} flag' QuickStart.md
}

patch () {
	sed -i "/$3/{
    s/ff//g
    r $2
}" $1
}

wd=scratch
mkdir -p $wd
{
	block 0
	echo
	block 1
	echo
	block 2
	echo
	block 3
} > $wd/1.fold
block 4 > $wd/1.fold.rf
block 5 > $wd/1.fold.ex
block 6 >> $wd/1.fold.ex
block 7 > $wd/1.fold.init
patch $wd/1.fold $wd/1.fold.rf "# TODO: fetch operands from registers"
patch $wd/1.fold $wd/1.fold.ex "# TODO: carry out operation, write result to register"
patch $wd/1.fold $wd/1.fold.init "var wm"

cat > $wd/linker.ld <<EOF
ENTRY(_start)

SECTIONS {
	. = 0x0;
	.text : { _start = .; *(.text .text.*) }
	.data : { *(.data .rdata .data.* .rdata.*) }
	. = ALIGN(4);
	_bss_start =  .;
	.bss : { *(.bss .bss.*) }
	. = ALIGN(4);
	_bss_end = .;
	. = _start + 16384;
	_stack_top = .;
}
EOF

cat > $wd/crt0.S <<EOF
.extern _stack_top
.globl _start, enable_irqs

_start:
	la sp, _stack_top
	call clear_bss
	call main
end:
	wfi
	j end
EOF

cat > $wd/crt1.c <<EOF
extern unsigned int _bss_start;
extern unsigned int _bss_end;
void clear_bss()
{
	for (unsigned int *p = &_bss_start; p != &_bss_end; p++)
		*p = 0;
}
EOF

cat > $wd/main.c <<EOF
int gcd(int a, int b)
{
	if (a == b) return a;
	else if (a > b) return gcd(a - b, b);
	else return gcd(b - a, a);
}

void main()
{
	int ret = gcd(14, 49);
	while (1);
}
EOF

cat > $wd/to_tsv.py <<EOF
import sys
while True:
	l = sys.stdin.buffer.read(4)
	if not l:
		break
	print("%d\t%d\t%d\t%d" % (l[0], l[1], l[2], l[3]))
EOF

ARCH="-mabi=ilp32e -march=rv32ezicsr -mstrict-align"
CFLAGS="$ARCH -MD -Wbuiltin-declaration-mismatch -I. -DNDEBUG -fdata-sections -ffunction-sections"
ASFLAGS="$ARCH"
riscv32-none-elf-gcc $ARCH -c -o $wd/crt0.o $wd/crt0.S
riscv32-none-elf-gcc $ARCH $CFLAGS -c -o $wd/crt1.o $wd/crt1.c
riscv32-none-elf-gcc $ARCH $CFLAGS -c -o $wd/main.o $wd/main.c
riscv32-none-elf-gcc -o $wd/program_image.elf -nostdlib -T $wd/linker.ld $wd/*.o
riscv32-none-elf-objcopy --pad-to 512 -O binary $wd/program_image.elf $wd/program_image.bin
python3 $wd/to_tsv.py < $wd/program_image.bin > $wd/program_image.tsv

{
	cd $wd
	PYTHONPATH="../../:$PYTHONPATH" python3 -m fold.machinecode -e 1.fold | head -n 150
	cd ..
}

cp $wd/1.fold $wd/2.fold
sed -i "s/i = memio(pc, undef, undef, 0);/i = memio(pc, undef, undef, 0); delay(1);/" $wd/2.fold
head -n -2 $wd/2.fold > $wd/2.fold.tmp && mv $wd/2.fold.tmp $wd/2.fold
echo -e "	delay(1); # end-of-loop delay\n}" >> $wd/2.fold
cp $wd/2.fold $wd/3.fold

fold_n_simulate () {
	cd $wd
	fold-yosys -Q -m ../../build/fold.so -m ../../fold.logic.frontend.py -p "read_fold $1; fold_synth; read_verilog -sv ../../support/mutex_assert.sv; hierarchy -top top; proc; memory_nordff; sim -q -n 100 -assert -clock clk -reset rst"	
	cd ..
}

sed -i "s/var wm \[128, 4, 8\]/var wm [2, 4, 8]/" $wd/2.fold
sed -i "s/wm = read_tsv!('program_image.tsv');//" $wd/2.fold
#fold_n_simulate 2.fold

sed -i "s/wm = read_tsv!('program_image.tsv');/\` meminit.\nwm = read_tsv!('program_image.tsv');/" $wd/3.fold
sed -i "s/var wm \[128, 4, 8\] mut;/var wm [128, 4, 8] mut; \` mem./" $wd/3.fold
sed -i "s/var regs \[16, 32\] mut;/var regs [16, 32] mut; \` mem./" $wd/3.fold
sed -i "s/r1s = r1; r2s = r2;/r1s = r1; r2s = r2;\n\tdelay(1);/" $wd/3.fold
sed -i "s/wb(r); # load write-back/delay(1);\n\t\twb(r); # load write-back/" $wd/3.fold
fold_n_simulate 3.fold

cp $wd/3.fold $wd/4.fold
sed -i "s/wb(r); # load write-back/wb(r); # load write-back\n\t\tdelay(-1);\n\t\textra_delay1 = 1;/" $wd/4.fold
sed -i "s/} else if opcode == JAL {/\textra_delay1 = 1;\n\t} else if opcode == JAL {/" $wd/4.fold
sed -i "s/} else if opcode == JALR {/\tdelay(2);\n\t} else if opcode == JALR {/" $wd/4.fold
sed -i "s/} else if opcode == OP || opcode == OP_IMM {/\tdelay(2);\n\t} else if opcode == OP || opcode == OP_IMM {/" $wd/4.fold
sed -i "s/if cond { pc = pc_sample + imm_b; }/if cond { pc = pc_sample + imm_b; }\n\t\tdelay(2);/" $wd/4.fold
sed -i "s/delay(1); # end-of-loop delay/delay(-1); # end-of-loop delay/" $wd/4.fold
sed -i "s/# TODO: fetch operands from registers/if extra_delay1 {\n\t\tdelay(1);\n\t}\n\textra_delay1 = 0;\n\t# TODO: fetch operands from registers/" $wd/4.fold
sed -i "s/pc = 0;/pc = 0;\nvar extra_delay1 [1] mut;\nextra_delay1 = 0;/" $wd/4.fold
sed -i "s/r1 = regs\[rs1\];/\` transp.\n\tr1 = regs[rs1];/" $wd/4.fold
sed -i "s/r2 = (opcode != OP_IMM) ? regs\[rs2\] : imm_i;/\` transp.\n\tr2 = (opcode != OP_IMM) ? regs[rs2] : imm_i;/" $wd/4.fold
fold_n_simulate 4.fold
