// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct StaticAssertPass : Pass {
	StaticAssertPass() : Pass("static_assert", "check on asserts with constant inputs") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing STATIC_ASSERT pass. (visit asserts with constant inputs)\n");

		size_t argidx;
		bool assert_none = false;
		bool remove = false;
		bool fully_disabled = false;
		bool always_failing = false;
		bool always_passing = false;
		bool undef_enable = false;
		bool undef_input = false;
		bool any = false;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-assert-none")
				assert_none = true;
			else if (args[argidx] == "-remove")
				remove = true;
			else if (args[argidx] == "-fully_disabled")
				fully_disabled = true;
			else if (args[argidx] == "-always_failing")
				always_failing = true;
			else if (args[argidx] == "-always_passing")
				always_passing = true;
			else if (args[argidx] == "-undef_enable")
				undef_enable = true;
			else if (args[argidx] == "-undef_input")
				undef_input = true;
			else if (args[argidx] == "-any")
				any = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		int nhits = 0;

		for (auto m : d->selected_modules())
		for (auto cell : m->selected_cells()) {
			if (cell->type != ID($assert))
				continue;

			SigSpec EN = cell->getPort(ID::EN);
			SigSpec A = cell->getPort(ID::A);

			bool matches = false;
			bool misses = false;

			auto criteria = [&](bool a) {
				if (a)
					matches = true;
				else
					misses = true;
			};

			if (fully_disabled) criteria(EN.is_fully_zero());
			if (always_failing) criteria(A.is_fully_zero());
			if (always_passing) criteria(A.is_fully_ones());
			if (undef_enable)   criteria(EN.is_fully_undef());
			if (undef_input)    criteria(A.is_fully_undef());

			if (!matches || (misses && !any))
				continue;

			log("Found assertion cell '%s' (A = %s, EN = %s)\n",
				log_id(cell->name), log_signal(cell->getPort(ID::A)),
				log_signal(cell->getPort(ID::EN)));

			if (assert_none)
				log_cell(cell, "failing cell: ");

			if (remove)
				m->remove(cell);
			nhits++;
		}

		if (assert_none && nhits > 0)
			log_error("Found %d assertion cells meeting specified criteria.\n", nhits);
	}
} StaticAssertPass;

PRIVATE_NAMESPACE_END
