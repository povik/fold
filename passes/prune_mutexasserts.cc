// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"
#include "kernel/rtlil.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct PruneMutexasserts : Pass {
	PruneMutexasserts() : Pass("prune_mutexasserts", "prune zero bits of mutex asserts") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing PRUNE_MUTEXASSERTS pass. (prune mutex assert inputs)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		int nhits = 0;

		for (auto m : d->selected_modules()) {
			for (auto cell : m->selected_cells()) {
				if (cell->type != ID(MUTEX_ASSERT))
					continue;

				SigSpec a = cell->getPort(ID::A);

				for (int i = a.size() - 1; i >= 0; i--)
				if (a[i] == SigBit(State::S0))
					a.remove(i);

				int nhits_here = cell->getPort(ID::A).size() - a.size();
				if (!nhits_here)
					continue;

				nhits += nhits_here;
				cell->setPort(ID::A, a);
				cell->setParam(ID::WIDTH, a.size());
			}
		}

		log("Pruned %d zero bits on input of mutex asserts.\n", nhits);
	}
} PruneMutexasserts;

PRIVATE_NAMESPACE_END
