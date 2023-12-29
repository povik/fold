// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/sigtools.h"
#include "kernel/register.h"
#include "kernel/yosys.h"

#include <algorithm>
#include <random>

USING_YOSYS_NAMESPACE

struct ScriptPrimedPass : Pass {
	ScriptPrimedPass() : Pass("script'", "execute commands from file, fall to shell on error") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SCRIPT_PRIMED pass.\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}

		if (args.size() != 2)
			log_cmd_error("Missing filename.\n");
		
		log_cmd_error_throw = true;
		try {
			run_frontend(args[1], "script", d);
		} catch(...) {
			log("Recovering from error encountered when running script '%s'.\n", args[1].c_str());
			shell(d);
		}
	}
} ScriptPrimedPass;

/* least upper bit state, for some sense of upper */
State lubit(State a, State b)
{
	return a == b ? a : State::Sx;
}

struct Connbgval : Pass {
	Connbgval() : Pass("connbgval", "copy background value on connected wires") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing CONNBGVAL pass. (copy background value)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			bool inconsistent = false;
			SigMap sigmap(m);
			dict<SigBit, State> bgval_attr;

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

			for (auto wire : m->selected_wires()) {
				bool nonempty = false;
				Const attr_val(State::Sx, wire->width);
				SigSpec mapped = sigmap(wire);

				for (int i = 0; i < mapped.size(); i++)
				if (bgval_attr.count(mapped[i])) {
					attr_val[i] = bgval_attr[mapped[i]];
					nonempty = true;
				}
				if (nonempty)
					wire->attributes[ID(seer.background_value)] = attr_val;
			}
			(void) inconsistent;
		}
	}
} Connbgval;

struct ShufflepmuxPass : Pass {
	ShufflepmuxPass() : Pass("shufflepmux", "shuffle the ordering of $pmux cases") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SHUFFLEPMUX pass. (shuffle $pmux)\n");

		size_t argidx;
		bool reverse = false;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-reverse")
				reverse = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		std::random_device rd;
    	std::mt19937 g(rd());

		for (auto m : d->selected_modules()) {
			for (auto cell : m->selected_cells()) {
				if (cell->type != ID($pmux))
					continue;

				std::vector<SigSig> cases;
				int width = cell->getParam(ID::WIDTH).as_int();
				int ncases = cell->getParam(ID::S_WIDTH).as_int();
				SigSpec S = cell->getPort(ID::S), B = cell->getPort(ID::B);
				for (int i = 0; i < ncases; i++)
					cases.emplace_back(S[i], B.extract(i * width, width));
 
 				if (reverse)
 					std::reverse(cases.begin(), cases.end());
    			else
    				std::shuffle(cases.begin(), cases.end(), g);

    			S = {}; B = {};
    			for (auto pair : cases) {
    				S.append(pair.first);
    				B.append(pair.second);
    			}

    			cell->setPort(ID::S, S);
    			cell->setPort(ID::B, B);
			}
		}
	}
} ShufflepmuxPass;
