// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct PropConstmuxesPass : Pass {
	PropConstmuxesPass() : Pass("prop_constmuxes", "propagate constants through partially-undef muxes") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing PROP_CONSTMUXES pass. (propagate constants through partially-undef muxes)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			SigMap sigmap(m);

			for (auto cell : m->selected_cells()) {
				if (!cell->type.in(ID($mux), ID($pmux)))
					continue;

				SigSpec a = sigmap(cell->getPort(ID::A));
				SigSpec b = sigmap(cell->getPort(ID::B));

				if (!a.is_fully_const() || !b.is_fully_const())
					continue;

				if (b.size() != a.size())
					continue;

				SigSpec out;

				if (a.is_fully_undef())
					out = b;
				else if (b.is_fully_undef())
					out = a;
				else
					continue;

				m->connect(cell->getPort(ID::Y), out);
				cell->setPort(ID::Y, SigSpec(State::Sx, out.size()));
			}
		}
	}
} PropConstmuxesPass;

PRIVATE_NAMESPACE_END
