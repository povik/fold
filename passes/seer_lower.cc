// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <limits.h>

#include "kernel/yosys.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

Const sigconst_besteffort(SigSpec sig)
{
	Const ret(State::Sx, sig.size());

	for (int i = 0; i < sig.size(); i++)
	if (!sig[i].wire)
		ret[i] = sig[i].data;

	return ret;
}

struct SeerLowerPass : Pass {
	SeerLowerPass() : Pass("seer_lower", "lower seers to register chains") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_LOWER pass. (lower seers)\n");

		size_t argidx;
		int cutoff = std::numeric_limits<int>::max();
		bool zero_reset = false;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-cutoff" && argidx + 1 < args.size())
				cutoff = atoi(args[++argidx].c_str());
			else if (args[argidx] == "-zero_reset")
				zero_reset = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			Wire *rst = m->wire(ID(rst));
			Wire *clk = m->wire(ID(clk));
			if (!clk || !rst || clk->width != 1 || rst->width != 1 \
				|| !clk->port_input || !rst->port_input) {
				log("Skipping module '%s' as it doesn't have both 'clk' and 'rst' ports.\n",
					log_id(m->name));
				continue;
			}

			SigMap bgval_attr(m);
			for (auto wire : m->selected_wires())
			if (wire->has_attribute(ID(seer.background_value))) {
				Const attr_val = wire->attributes[ID(seer.background_value)];

				for (int i = 0; i < attr_val.size(); i++) {
					SigBit wirebit = SigSpec(wire)[i];

					if (attr_val[i] == State::Sx)
						continue;

					if (!bgval_attr(wirebit).is_wire())
						continue; // we have a background value for this wire already

					bgval_attr.add(wirebit, attr_val[i]);
				}
			}

			std::vector<Cell *> to_remove;
			for (auto cell : m->selected_cells()) {
				if (cell->type != ID(SEER)) continue;
				int offset = cell->getParam(ID(OFFSET)).as_int(true);
				if (offset > 0 || offset < -cutoff) continue;
				int width = cell->getParam(ID(WIDTH)).as_int(false);

				Const reset_val = zero_reset ? Const(State::S0, width) \
								  : sigconst_besteffort(bgval_attr(cell->getPort(ID::Y)));

				if (offset == 0) {
					m->connect(cell->getPort(ID::Y), cell->getPort(ID::A));
					to_remove.push_back(cell);
					continue;
				}

				SigSpec last_q = cell->getPort(ID::A);
				for (int steps = -offset; steps > 0; steps--) {
					SigSpec next_q = steps > 1 ? m->addWire(NEW_ID, width) : cell->getPort(ID::Y);
					if (reset_val.is_fully_undef())
						m->addDff(NEW_ID, clk, last_q, next_q);
					else
						m->addSdff(NEW_ID, clk, rst, last_q, next_q, reset_val);
					last_q = next_q;
				}

				to_remove.push_back(cell);
			}

			for (auto c : to_remove)
				m->remove(c);
		}
	}
} SeerLowerPass;

PRIVATE_NAMESPACE_END
