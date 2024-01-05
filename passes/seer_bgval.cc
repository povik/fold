// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/celltypes.h"
#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

bool const_contained(Const outer, Const inner)
{
	log_assert(outer.size() == inner.size());
	for (int i = 0; i < outer.size(); i++)
		if (outer[i] != State::Sx && outer[i] != inner[i])
			return false;
	return true;
}

/* least upper bit state, for some sense of upper */
State lubit(State a, State b)
{
	return a == b ? a : State::Sx;
}

Const evalsig(dict<SigBit,State> &state, SigSpec signal)
{
	Const ret(State::Sx, signal.size());
	for (int i = 0; i < signal.size(); i++)
	if (state.count(signal[i]))
		ret[i] = state[signal[i]];
	return ret;
}

void spread_bgval(Module *m, CellTypes &ct, bool assert_consistence, bool writeback)
{
	bool inconsistent = false;
	SigMap sigmap(m);
	dict<SigBit,State> bgval_attr;

	for (auto wire : m->selected_wires())
	if (wire->has_attribute(ID(seer.background_value))) {
		Const attr_val = wire->attributes[ID(seer.background_value)];
		SigSpec wiresig = sigmap(wire);

		for (int i = 0; i < attr_val.size(); i++) {
			SigBit wirebit = wiresig[i];
			State attrbit = attr_val[i];

			if (attrbit == State::Sx)
				continue;

			if (bgval_attr.count(wirebit) && bgval_attr[wirebit] != attrbit) {
				log_warning("Inconsistent duplicate attribute on bit %s: %s and %s\n",
							log_signal(wirebit), log_signal(attrbit),
							log_signal(bgval_attr[wirebit]));
				inconsistent = true;
				attrbit = lubit(attrbit, bgval_attr[wirebit]);
			}

			bgval_attr[wirebit] = attrbit;
		}
	}

	dict<SigBit,State> bgval(bgval_attr);
	dict<SigBit,State> bgval_computed;
	bgval[SigBit(State::S0)] = State::S0;
	bgval[SigBit(State::S1)] = State::S1;

	while (true) {
		log_debug("Pass\n");

		bool did_something = false;

		auto set_computed = [&](SigSpec port, Const result){
			log_assert(result.size() == port.size());
			for (int i = 0; i < result.size(); i++) {
				if (!port[i].is_wire()) continue;

				bgval_computed[port[i]] = result[i];

				if (!bgval_attr.count(port[i])) {
					if (bgval[port[i]] != result[i])
						did_something = true;
					bgval[port[i]]= result[i];
				}
	 		}
		};

		for (auto cell : m->selected_cells()) {
			SigSpec port_y;
			Const a, b, c, s, result, old_val;

			if (yosys_celltypes.cell_evaluable(cell->type))
				goto evaluable;
			if (cell->type.in(ID($ff))) {
				set_computed(
					sigmap(cell->getPort(ID::Q)),
					evalsig(bgval, sigmap(cell->getPort(ID::D)))
				);
				continue;
			} else if (cell->type == ID(IMPORT)) {
				set_computed(
					sigmap(cell->getPort(ID::Q)),
					Const(cell->getParam(ID(ZEROED)).as_bool() ? State::S0 : State::Sx,
						  cell->getParam(ID::WIDTH).as_int())
				);
				continue;
			} else if (cell->type.in(ID(SEER))) {
				set_computed(
					sigmap(cell->getPort(ID::Y)),
					evalsig(bgval, sigmap(cell->getPort(ID::A)))
				);
				continue;
			} else if (cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE))) {
				set_computed(
					sigmap(cell->getPort(ID(AY))),
					evalsig(bgval, sigmap(cell->getPort(ID::A)))
				);
				set_computed(
					sigmap(cell->getPort(ID(BY))),
					evalsig(bgval, sigmap(cell->getPort(ID::B)))
				);
				continue;
			} else {
				bool has_output = false;
				for (auto conn : cell->connections()) {
					if (ct.cell_output(cell->type, conn.first))
						has_output = true;
				}

				if (has_output)
					goto unsupported;
				else
					continue;
			}

evaluable:
			if (cell->type.in(ID($assert)))
				continue;

			if (!cell->hasPort(ID::Y))
				log_error("Evaluable cell '%s' type %s missing Y port",
						  log_id(cell->name), log_id(cell->type));

			port_y = sigmap(cell->getPort(ID::Y));
			if (cell->hasPort(ID::A)) a = evalsig(bgval, sigmap(cell->getPort(ID::A)));
			if (cell->hasPort(ID::B)) b = evalsig(bgval, sigmap(cell->getPort(ID::B)));
			if (cell->hasPort(ID::C)) c = evalsig(bgval, sigmap(cell->getPort(ID::C)));
			if (cell->hasPort(ID::S)) s = evalsig(bgval, sigmap(cell->getPort(ID::S)));

			if (!cell->hasPort(ID::A) || !cell->hasPort(ID::Y) || cell->hasPort(ID::D))
				goto unsupported;

			if (cell->hasPort(ID::S) && cell->hasPort(ID::B) &&
					!cell->hasPort(ID::C))
				result = CellTypes::eval(cell, a, b, s);
			else if (cell->hasPort(ID::C) && !cell->hasPort(ID::S))
				result = CellTypes::eval(cell, a, b, c);
			else if (!cell->hasPort(ID::C) && !cell->hasPort(ID::S))
				result = CellTypes::eval(cell, a, b);
			else
				goto unsupported;

			log_assert(port_y.size() == result.size());

			old_val = evalsig(bgval_computed, port_y);
			if (!const_contained(old_val, result))
				log_error("Reevaluated '%s' (%s) from %s to %s, that shouldn't have happened.\n",
						  log_id(cell->name), log_id(cell->type),
						  log_signal(old_val), log_signal(result));

			log_debug("%s a %s b %s old_val %s result %s\n",
					  log_id(cell->name), log_signal(a), log_signal(b),
					  log_signal(old_val), log_signal(result));

			set_computed(port_y, result);
			continue;

unsupported:
			log_debug("Unsupported cell type %s\n", log_id(cell->type));
		}

		if (!did_something)
			break;
	}

	for (auto attrpair : bgval_attr) {
		if (!bgval_computed.count(attrpair.first))
			continue;

		if (lubit(attrpair.second, bgval_computed[attrpair.first]) \
				!= attrpair.second) {
			log_warning("Inconsistent background value on %s: computed %s attribute %s\n",
						log_signal(attrpair.first), log_signal(bgval_computed[attrpair.first]),
						log_signal(attrpair.second));
			inconsistent = true;
		}
	}

	if (assert_consistence && inconsistent)
		log_error("Background value attributes are inconsistent.\n");

	if (writeback) {
		for (auto wire : m->selected_wires()) {
			bool nonempty = false;
			Const attr_val(State::Sx, wire->width);
			SigSpec mapped = sigmap(wire);

			for (int i = 0; i < mapped.size(); i++)
			if (bgval.count(mapped[i])) {
				attr_val[i] = bgval[mapped[i]];
				nonempty = true;
			}

			if (nonempty)
				wire->attributes[ID(seer.background_value)] = attr_val;
		}
	}
}

struct SeerSpreadbg : Pass {
	SeerSpreadbg() : Pass("seer_spreadbg", "spread background value around the circuit") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_SPREAD_BGVAL pass. (spread background values)\n");

		bool assert_consistence = false;
		bool writeback = false;
		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			auto arg = args[argidx];
			if (arg == "-assert_consistence")
				assert_consistence = true;
			else if (arg == "-writeback")
				writeback = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		CellTypes ct(d);

		for (auto m : d->selected_modules())
			spread_bgval(m, ct, assert_consistence, writeback);
	}
} SeerSpreadbg;

PRIVATE_NAMESPACE_END
