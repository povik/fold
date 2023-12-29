// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct PatchCrankPass : Pass {
	PatchCrankPass() : Pass("patch_crank", "patch the crank for Fold synthesis") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing PATCH_CRANK pass. (patch the crank signal)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		if (d->selected_modules().size() != 1)
			log_error("Pass operates on a single module");

		auto m = d->selected_modules()[0];

		Wire *clk = m->wire(ID(clk));
		Wire *rst = m->wire(ID(rst));
		Wire *crank = m->wire(ID(crank));

		if (!clk || !rst || !crank)
			log_error("Missing one of the following wires: clk, rst, crank\n");

		crank->port_input = false;
		m->fixup_ports();
		m->addSdff(NEW_ID, clk, rst, Const(0, 1),
				   crank, Const(1, 1));
	}
} PatchCrankPass;

PRIVATE_NAMESPACE_END
