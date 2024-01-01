// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <vector>

#include "kernel/sigtools.h"
#include "kernel/celltypes.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

#include "shared.h"
#include "timetravel.h"

struct InitplanWorker {
	Design *d;
	Module *m;
	CellTypes ct;
	SigMap sigmap;
	timetravel_module ttm;
	seer_trees_database_t db;

	dict<IdString, pool<timetravel_node>> domain_push;

	timetravel_node initiator;
	dict<timetravel_node, int> assigns;

	pool<timetravel_node> to_move;
	int requested_extent;

	pool<IdString> to_select;

	struct driver_info {
		timetravel_node node;
		timetravel_module::portbit port;
	};

	dict<SigBit, driver_info> drivers;

	dict<SigBit, SigBit> conmap;
	bool conns_indexed = false;

	bool select_offenders = false;
	bool best_effort = false;

	InitplanWorker(Design *d, Module *m)
			: d(d), m(m), ct(d), sigmap(m),
			  ttm(m, sigmap, ct, true), db(sigmap) {
		for (auto cell : m->cells())
			db.register_cell(cell);
		db.traverse_sources();

		for (auto node : ttm.nodes) {
			ttm.visit_outputs(node, [&](SigBit bit, timetravel_module::portbit port) {
				drivers[bit] = driver_info{node, port};
			});

			assigns[node] = node.get_timetravel_attribute();

			if (node.is_timeportal() && node.domain_constrained_from_below())
				domain_push[node.timetravel_domain()].insert(node);
		}
	}

	void run()
	{
		int i = 0;

		while (true) {
			log_debug("Planning (iteration %d)...\n", i);
			if (!plan()) break;
			log_debug("Moving (iteration %d)...\n", i++);
			if (!move()) break;
		}

		save();
	}

	bool move() {
		if (to_move.empty())
			return false;

		int extent = requested_extent; // std::numeric_limits<int>::max();

		for (auto node : to_move) {
			if (node.is_seer()) continue;

			ttm.visit_inputs(node, [&](SigBit bit, YS_MAYBE_UNUSED timetravel_module::portbit port) {
				auto upstream = db.lookup(bit);

				if (!drivers.count(upstream.bit))
					return;

				auto driver = drivers[upstream.bit];

				if (to_move.count(driver.node))
					// We can ignore a co-moving driver
					return;

				extent = std::min(extent, -upstream.offset - assigns[driver.node] + assigns[node]);
			});

			if (node.is_backedge() && node.bank == ID::A)
				extent = std::min(extent, assigns[node] - assigns[node.partner()]);
		}

		log("Moving %zu nodes by %d.\n", to_move.size(), extent);
		assert(extent > 0);

		for (auto node : to_move)
			assigns[node] -= extent;

		return true;
	}

	void index_conns()
	{
		for (auto &it : m->connections()) {
			log_assert(it.first.size() == it.second.size());
			for (int i = 0; i < it.first.size(); i++)
				conmap[it.first[i]] = it.second[i];
		}
		conns_indexed = true;
	}

	void select_bitpath(timetravel_node node,
						timetravel_module::portbit portbit)
	{
		if (!conns_indexed)
			index_conns();

		/*
		 * Instead of simply snatching the bit given by visit_inputs
		 * we need to peek into the cell/module directly to get a bit
		 * that hasn't been sigmapped.
		 */
		SigBit bit = node.is_perimeter() ?
					 SigSpec(m->wire(portbit.label))[portbit.offset] :
					 node.cell->getPort(portbit.label)[portbit.offset];

		if (bit.is_wire())
			to_select.insert(bit.wire->name);

		while (conmap.count(bit) || db.sources.count(sigmap(bit))) {
			while (conmap.count(bit)) {
				bit = conmap[bit];
				if (bit.is_wire())
					to_select.insert(bit.wire->name);
			}

			while (db.drivers.count(sigmap(bit))) {
				seer_trees_database_t::driver_t driver = 
					db.drivers[sigmap(bit)];
				bit = driver.cell->getPort(ID(A))[driver.bitpos];
				if (bit.is_wire())
					to_select.insert(bit.wire->name);
				to_select.insert(driver.cell->name);
			}
		}
	}

	void log_node_insatiable(timetravel_node node)
	{
		if (node.cell) {
			std::string src;
			if (node.cell->has_attribute(ID::src))
				src += "at " + node.cell->get_src_attribute();
			log("\tNode: %s (type %s) %s\n", log_id(node.name()), log_id(node.cell->type), src.c_str());
		} else {
			log("\tNode: %s\n", log_id(node.name()));
		}
		if (node.cell)
			to_select.insert(node.cell->name);
	}

	bool plan_push(timetravel_node node)
	{
		if (node == initiator) {
			/* Uh oh */
			log("Insatiable cycle detected:\n");
			log_node_insatiable(node);
			return false;
		}

		if (to_move.count(node)) {
			/*
		     * If this node is staged to be moved, we must have
			 * visited it already and our work is done.
			 */
			return true;
		}

		to_move.insert(node);

		bool bad = false;

		ttm.visit_inputs(node, [&](SigBit bit, timetravel_module::portbit port) {
			if (bad)
				return;

			auto upstream = db.lookup(bit);
			if (!drivers.count(upstream.bit))
				return;
			auto driver = drivers[upstream.bit];

			if (upstream.offset + assigns[driver.node] - assigns[node] < 0)
				return; /* There's leeway */

			if (!plan_push(driver.node)) {
				log("\t  %s[%d] --[ %d ]--> %s[%d]\n",
					log_id(driver.port.label), driver.port.offset,
					upstream.offset, log_id(port.label), port.offset);
				log_node_insatiable(node);
				select_bitpath(node, port);
				bad = true;
			}
		});

		if (node.is_timeportal() && node.domain_constrained_from_above())
		for (auto dnode : domain_push[node.timetravel_domain()])
		if (!plan_push(dnode)) {
			log("\t  (domain %s)\n", log_id(node.timetravel_domain()));
			log_node_insatiable(node);
			bad = true;
		}

		if (node.is_backedge() && node.bank == ID::A \
				&& assigns[node.partner()] >= assigns[node])
		if (!plan_push(node.partner())) {
			log("\t  (backedge)\n");
			log_node_insatiable(node);
			bad = true;
		}

		if (bad)
			return false;

		return true;
	}

	bool plan()
	{
		bool bad = false;
		to_move.clear();

		requested_extent = 0;

		for (auto node : ttm.nodes) {
			if (node.is_seer()) continue;

			if (to_move.count(node)) {
				/*
				 * If this node is staged to be moved, we must have
				 * visited it already and our work is done.
				 */
				continue;
			}

			initiator = node;

			ttm.visit_inputs(node, [&](SigBit bit, timetravel_module::portbit port) {
				auto upstream = db.lookup(bit);

				if (!drivers.count(upstream.bit))
					return;

				auto driver = drivers[upstream.bit];

				if (upstream.offset + assigns[driver.node] - assigns[node] <= 0)
					return;

				requested_extent = std::max(requested_extent,
											upstream.offset + assigns[driver.node] - assigns[node]);

				log_debug("Need push due to: %s[%d] --[ %d ]--> %s[%d]\n",
						  log_id(driver.port.label), driver.port.offset,
						  upstream.offset, log_id(port.label), port.offset);

				if (!plan_push(driver.node)) {
					log("\t  %s[%d] --[ %d ]--> %s[%d]\n",
						log_id(driver.port.label), driver.port.offset,
						upstream.offset, log_id(port.label), port.offset);
					log_node_insatiable(node);
					select_bitpath(node, port);
					bad = true;
				}
			});
		}

		if (bad) {
			if (select_offenders) {
				RTLIL::Selection sel(false);
				for (auto member : to_select)
					sel.selected_members[ttm.m->name].insert(member);
				d->selection_stack.back() = sel;
				d->selection_stack.back().optimize(d);
				return false;
			} else {
				d->scratchpad_set_bool("seer.insatiable", true);

				if (!best_effort)
					log_error("Can't prepare an initial plan of timetravel due to insatiable cycles.");
				return false;
			}
		}

		return !bad;
	}

	void save()
	{
		int pivot = assigns[timetravel_node::perimeter];

		for (auto node : ttm.nodes) {
			if (node.is_seer()) continue;
			node.set_timetravel_attribute(assigns[node] - pivot);
		}
	}
};

struct SeerInitplanPass : Pass {
	SeerInitplanPass() : Pass("seer_initplan", "find an initial timetravel plan") {}
	void help() override
	{
		log("\n");
		log("    seer_initplan [-select_offenders] [-best_effort]");
		log("\n");
	}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_INITPLAN pass. (plan initial timetravel)\n");

		size_t argidx;
		bool select_offenders = false;
		bool best_effort = false;
		for (argidx = 1; argidx < args.size(); argidx++) {
			auto arg = args[argidx];
			if (arg == "-select_offenders")
				select_offenders = true;
			else if (arg == "-best_effort")
				best_effort = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			InitplanWorker w(d, m);
			w.select_offenders = select_offenders;
			w.best_effort = best_effort;
			w.run();
		}
	}
} SeerInitplanPass;

PRIVATE_NAMESPACE_END
