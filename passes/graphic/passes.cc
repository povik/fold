// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <iostream>

#include "graph.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct ImmutlinksPass : Pass {
	ImmutlinksPass() : Pass("immutlinks", "immutlinks manipulation") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMMUTLINKS pass.\n");

		bool dump = false;
		bool writeback = false;
		bool keep_hots = false;

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-dump") {
				dump = true;
				continue;
			}
			if (args[argidx] == "-writeback") {
				writeback = true;
				continue;
			}
			if (args[argidx] == "-keep_hots") {
				keep_hots = true;
				continue;
			}
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			log("Visiting module %s.\n", log_id(m->name));
			Immutlinks links;
			links.parse(m);
			links.index();

			if (keep_hots) {
				for (auto node : links.nodes)
				for (auto const &edge : links.edges[node])
				if (edge.dir && edge.hot.is_wire())
					edge.hot.as_wire()->attributes[ID::keep] = Const(1, 1);
			}

			if (dump)
				links.dump(std::cerr);

			if (writeback)
				links.save(m);
		}
	}
} ImmutlinksPass;

struct ImmutlinksCleanPass : Pass {
	ImmutlinksCleanPass() : Pass("immutlinks_clean", "clean the immutlinks graph") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMMUTLINKS_CLEAN pass. (clean immutlinks graph)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			log("Visiting module %s.\n", log_id(m->name));
			SigMap sigmap(m);
			Immutlinks links;
			links.parse(m);
			links.cleanup_hots(sigmap);

			::hashlib::pool<Immutnode *> queue;
			for (auto cell : m->cells()) {
				if (cell->type.in(ID(VAR_SET), ID(VAR_GET))) {
					queue.insert(cell_immutnode(cell, links));
				} else if (cell->type == ID(IMPORT)) {
					queue.insert(cell_immutnode(cell, links, ID(FROM_NODE)));
					queue.insert(cell_immutnode(cell, links, ID(TO_NODE)));
				}
			}

			for (auto &pair : links.edges)
			for (auto &edge : pair.second) {
				if (!edge.dir || edge.in_spantree || edge.phantom_threshold)
					continue;
				queue.insert(edge.from);
				queue.insert(edge.to);
			}

			::hashlib::pool<Immutnode *> used;
			while (!queue.empty()) {
				Immutnode *head = queue.pop();
				used.insert(head);
				for (auto &edge : links.edges[head])
				if (!edge.dir && !used.count(edge.to) && edge.hot != State::S0 && edge.to->en != State::S0)
					queue.insert(edge.to);
			}

			int nunused = 0;
			for (auto node : links.nodes)
			if (!used.count(node)) {
				log_debug("\tfound unused node: %s\n", log_id(node->id));
				nunused++;
				node->remove = true;
			}
			log("Cleaned up %d unused nodes.\n", nunused);
			links.save(m);
		}
	}
} ImmutlinksCleanPass;


struct ImselectPass : Pass {
	ImselectPass() : Pass("imselect", "select immutlinks control signals") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMSELECT pass. (select immutlinks control signals)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		RTLIL::Selection sel(false);

		for (auto m : d->selected_modules()) {
			Immutlinks links;
			links.parse(m);

			SigSpec hots;
			for (auto node : links.nodes) {
				if (!node->en.is_fully_const()) {
					log_assert(node->en.is_wire());
					sel.select(m, node->en.as_wire());
				}
				for (auto const &edge : links.edges[node])
				if (edge.dir && !edge.hot.is_fully_const()) {
					log_assert(edge.hot.is_wire());
					sel.select(m, edge.hot.as_wire());
				}
			}
		}

		d->selection_vars[ID(immutlinks_ctl)] = sel;
	}
} ImselectPass;

struct SpantreeWalk : Pass {
	SpantreeWalk() : Pass("spantree_walk", "test immutlinks spantree walks") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
		}
		extra_args(args, argidx, d);

		Module *m = d->selected_modules()[0];
		log("Visiting module %s.\n", log_id(m->name));
		Immutlinks links;
		links.parse(m);
		links.index();
		links.dump(std::cerr);

		Immutnode *from = links.nodes_by_id[IdString(RTLIL::escape_id(args[1]))];
		Immutnode *to   = links.nodes_by_id[IdString(RTLIL::escape_id(args[2]))];

		for (auto edge : links.walk_spantree(from, to))
			Immutlinks::dump_edge(std::cout, edge);
	}
} SpantreeWalk;

struct ImpalettePass : Pass {
	ImpalettePass() : Pass("impalette", "build immutlinks hot signal palette") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMPALETTE pass. (build immutlinks palette)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			Immutlinks links;
			links.parse(m);

			SigSpec hots;
			for (auto node : links.nodes) {
				hots.append(node->en);
				for (auto const &edge : links.edges[node])
				if (edge.dir && edge.hot != SigBit(State::S1))
					hots.append(edge.hot);
			}

			Wire *wire = m->addWire(ID(impalette), hots.size());
			m->connect(wire, hots);

			int idx = 0;
			for (auto node : links.nodes) {
				node->en = SigBit(wire, idx++);
				for (auto &edge : links.edges[node])
				if (edge.dir && edge.hot != SigBit(State::S1))
					edge.hot = SigBit(wire, idx++);
			}
			log_assert(idx == wire->width);

			links.save(m);
		}
	}
} ImpalettePass;

PRIVATE_NAMESPACE_END
