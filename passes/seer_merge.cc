// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"

PRIVATE_NAMESPACE_BEGIN
USING_YOSYS_NAMESPACE

#include "shared.h"

void merge_seers(Module *m)
{
	SigMap sigmap(m);
	seer_trees_database_t db(sigmap);

	for (auto cell : m->selected_cells())
		db.register_cell(cell);
	db.traverse_sources();

	dict<int, SigSpec> new_levels;

	for (auto cell : m->selected_cells()) {
		if (cell->type != ID(SEER))
			continue;

		SigSpec a = cell->getPort(ID(A));
		SigSpec y = cell->getPort(ID(Y));
		int width = a.size();

		SigSpec to_split_off;

		for (int i = 0; i < width; i++)
		if (db.lookup(y[i]).bit != a[i]) {
			auto lookup = db.lookup(y[i]);
			to_split_off.append(a[i]);
			new_levels[lookup.offset].append(y[i]);
		}

		if (to_split_off.empty())
			continue;

		a.remove(to_split_off, &y);
		a.remove(to_split_off);
		cell->setPort(ID(A), a);
		cell->setPort(ID(Y), y);
		cell->setParam(ID(WIDTH), a.size());
	}

	for (auto level : new_levels) {
		int offset = level.first;
		level.second.sort_and_unify();

		for (auto chunk : level.second.chunks()) {
			if (!chunk.is_wire())
				continue;
			create_seer(m, NEW_ID, db.wide_lookup(chunk), chunk, offset);
		}
	}
}

struct SeerMergePass : Pass {
	SeerMergePass() : Pass("seer_merge", "merge consecutive seers") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_MERGE pass. (merge seers)\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			merge_seers(m);
		}
	}
} SeerMergePass;

PRIVATE_NAMESPACE_END
