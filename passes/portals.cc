// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"
#include "backends/rtlil/rtlil_backend.h"

PRIVATE_NAMESPACE_BEGIN
USING_YOSYS_NAMESPACE

void fixup_backedge_or_timeportal_cell(Cell *cell)
{
	cell->setParam(ID::A_WIDTH, cell->getPort(ID::A).size());
	cell->setParam(ID::B_WIDTH, cell->getPort(ID::B).size());
}

SigSpec extract_const(SigSpec dispatch, SigSpec target)
{
	SigSpec ret;

	log_assert(dispatch.size() == target.size());
	for (int i = dispatch.size() - 1; i >= 0; i--)
	if (!dispatch[i].wire)
		ret.append(target[i]);
	return ret;
}

void remove_const(SigSpec dispatch, SigSpec &target)
{
	log_assert(dispatch.size() == target.size());
	for (int i = dispatch.size() - 1; i >= 0; i--)
	if (!dispatch[i].wire)
		target.remove(i);
}

int propagate_const(SigMap &sigmap, Module *m, Cell *cell,
					IdString inp_id, IdString outp_id)
{
	SigSpec canary = sigmap(cell->getPort(inp_id));

	if (!canary.has_const())
		return 0;

	SigSpec a_sig = cell->getPort(inp_id);
	SigSpec y_sig = cell->getPort(outp_id);

	SigSig newconn = SigSig(
		extract_const(canary, y_sig),
		extract_const(canary, a_sig)
	);
	m->connect(newconn);

	remove_const(canary, a_sig);
	remove_const(canary, y_sig);

	cell->setPort(inp_id, a_sig);
	cell->setPort(outp_id, y_sig);

	return newconn.first.size();
}

struct BackedgeLowerPass : Pass {
	BackedgeLowerPass() : Pass("backedge_lower", "lower backedges to oracles") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing BACKEDGE_LOWER pass. (lower backedges to oracles)\n");

		IdString port_selection;
		size_t argidx;
		bool oracle = false, bypass = false;
		for (argidx = 1; argidx < args.size(); argidx++) {
			auto arg = args[argidx];
			if (arg == "-select_port" && argidx + 1 < args.size() \
					&& IdString(RTLIL::escape_id(args[++argidx])).in({ID::A, ID::B}))
				port_selection = IdString(RTLIL::escape_id(args[argidx]));
			else if (arg == "-oracle")
				oracle = true;
			else if (arg == "-bypass")
				bypass = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		if (!oracle && !bypass)
			log_error("One of -oracle or -bypass needs to be selected.\n");

		for (auto m : d->selected_modules())
		for (auto cell : m->selected_cells()) {
			if (cell->type != ID(BACKEDGE))
				continue;

			for (auto id : {ID::A, ID::B}) {
				if (!port_selection.empty() && id != port_selection)
					continue;
				if (!cell->getPort(id).size())
					continue;

				IdString counter_id = (id == ID::A) \
									  ? ID(AY) : ID(BY);
				auto a_sig = cell->getPort(id);
				auto y_sig = cell->getPort(counter_id);

				if (bypass) {
					m->connect(y_sig, a_sig);
				} else if (oracle) {
					auto oracle = m->addCell(NEW_ID, ID(ORACLE));
					oracle->setPort(ID::A, a_sig);
					oracle->setPort(ID::Y, y_sig);
					oracle->setParam(ID::WIDTH, y_sig.size());
				} else {
					log_assert(false && "No lowering mode");
				}

				cell->setPort(id, SigSpec());
				cell->setPort(counter_id, SigSpec());
				fixup_backedge_or_timeportal_cell(cell);
			}
		}
	}
} BackedgeLowerPass;

struct PortalConstPass : Pass {
	PortalConstPass() : Pass("portal_const", "propagate constant bits though portals (all kinds)") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing PORTAL_CONST pass. (propagate constant bits through portals)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			SigMap sigmap(m);
			int acc = 0;
			
			for (auto cell : m->selected_cells()) {
				int cell_hits = 0;

				if (cell->type == ID(TIMEPORTAL) || cell->type == ID(BACKEDGE)) {
					cell_hits += propagate_const(sigmap, m, cell, ID::A, ID(AY));
					cell_hits += propagate_const(sigmap, m, cell, ID::B, ID(BY));

					if (cell_hits)
						fixup_backedge_or_timeportal_cell(cell);
				} else if (cell->type.in(ID(ORACLE), ID(SEER))) {
					cell_hits = propagate_const(sigmap, m, cell, ID::A, ID::Y);

					if (cell_hits)
						cell->setParam(ID::WIDTH, cell->getPort(ID::A).size());
				} else if (cell->type.in(ID(IMPORT))) {
					cell_hits = propagate_const(sigmap, m, cell, ID::D, ID::Q);

					if (cell_hits)
						cell->setParam(ID::WIDTH, cell->getPort(ID::Q).size());
				} else {
					continue;
				}

				acc += cell_hits;
			}

			log("Propagated %d constant bits through portals.\n", acc);
		}
	}
} PortalConstPass;

struct PortalCheckPass : Pass {
	PortalCheckPass() : Pass("portal_check", "check on portal cells") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing PORTAL_CHECK pass. (check on timeportals)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		bool found_issues = false;
		for (auto m : d->selected_modules())
		for (auto cell : m->selected_cells()) {
			bool cell_bad = false;

			if (cell->type.in({ID(TIMEPORTAL), ID(BACKEDGE)})) {
				cell_bad |= require_param(cell, ID(A_WIDTH));
				cell_bad |= require_param(cell, ID(B_WIDTH));
				cell_bad |= require_port(cell, ID(A));
				cell_bad |= require_port(cell, ID(AY));
				cell_bad |= require_port(cell, ID(B));
				cell_bad |= require_port(cell, ID(BY));
			} else if (cell->type.in({ID(SEER), ID(ORACLE)})) {
				cell_bad |= require_param(cell, ID(WIDTH));
				cell_bad |= require_port(cell, ID(A));
				cell_bad |= require_port(cell, ID(Y));
			} else {
				continue;
			}

			if (cell_bad) {
				dump_cell(cell);
				found_issues = true;
				continue;
			}

			if (cell->type.in({ID(TIMEPORTAL), ID(BACKEDGE)})) {
				cell_bad |= check_size(cell, ID(A),  ID(A_WIDTH));
				cell_bad |= check_size(cell, ID(AY), ID(A_WIDTH));
				cell_bad |= check_size(cell, ID(B),  ID(B_WIDTH));
				cell_bad |= check_size(cell, ID(BY), ID(B_WIDTH));
			} else if (cell->type.in({ID(SEER), ID(ORACLE)})) {
				cell_bad |= check_size(cell, ID(A), ID(WIDTH));
				cell_bad |= check_size(cell, ID(Y), ID(WIDTH));
			}

			if (cell->type == ID(SEER))
				cell_bad |= require_param(cell, ID(OFFSET));

			if (cell_bad) {
				dump_cell(cell);
				found_issues = true;
				continue;
			}

		}

		if (found_issues)
			log_error("Found issues.\n");
	}

	void dump_cell(Cell *cell) {
		std::stringstream buf;
		RTLIL_BACKEND::dump_cell(buf, "  ", cell);
		log("%s\n", buf.str().c_str());
	}

	bool require_param(Cell *cell, IdString id) {
		if (!cell->hasParam(id)) {
			log("Cell %s of type %s is missing parameter %s.\n",
				log_id(cell->name), log_id(cell->type), log_id(id));
			return true;
		}

		return false;
	}

	bool require_port(Cell *cell, IdString id) {
		if (!cell->hasPort(id)) {
			log("Cell %s of type %s is missing port %s.\n",
				log_id(cell->name), log_id(cell->type), log_id(id));
			return true;
		}

		return false;
	}

	bool check_size(Cell *cell, IdString id, IdString param_id) {
		if (cell->getPort(id).size() != cell->getParam(param_id).as_int()) {
			log("Cell %s of type %s has badly sized connection on port %s.\n",
				log_id(cell->name), log_id(cell->type), log_id(id));
			return true;
		}

		return false;
	}
} PortalCheckPass;

struct TimeportalDropPass : Pass {
	TimeportalDropPass() : Pass("timeportal_drop", "remove timeportals") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing TIMEPORTAL_DROP pass. (remove timeportals)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			for (auto cell : m->selected_cells()) {
				if (cell->type != ID(TIMEPORTAL))
					continue;
				m->connect(cell->getPort(ID(AY)), cell->getPort(ID::A));
				m->connect(cell->getPort(ID(BY)), cell->getPort(ID::B));
				m->remove(cell);
			}
		}
	}
} TimeportalDropPass;

PRIVATE_NAMESPACE_END
