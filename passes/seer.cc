// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/celltypes.h"
#include "kernel/register.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE

#include "shared.h"

struct SeerStatPass : Pass {
	SeerStatPass() : Pass("seer_stat", "print seer statistics") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Printing seer statistics.\n");

		bool assert_max = false;
		bool assert_no_magic = false;
		bool mask_undef_mux = false;
		int assert_val;

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-assert-max") {
				assert_max = true;
				assert_val = atoi(args[++argidx].c_str());
				continue;
			}
			if (args[argidx] == "-assert-no-magic") {
				assert_no_magic = true;
				continue;
			}
			if (args[argidx] == "-mask_undef_mux") {
				mask_undef_mux = true;
				continue;
			}
			break;
		}
		extra_args(args, argidx, d);

		int nseers = 0;
		int nmagic_seers = 0;
		int nseerbits = 0;
		int nunique_seerbits = 0;
		int nunique_seerbits2 = 0;
		bool found_magic2 = false;

		CellTypes ct;
		ct.setup(d);

		for (auto m : d->selected_modules()) {
			SigMap sigmap(m);
			seer_trees_database_t db(sigmap);
			dict<SigBit,int> bitoffsets;

			for (auto cell : m->selected_cells()) {
				if (cell->type != ID(SEER))
					continue;

				int offset = cell->getParam(ID(OFFSET)).as_int(true);
				int width = cell->getParam(ID(WIDTH)).as_int();

				nseers++;
				if (offset > 0)
					nmagic_seers++;
				else {
					nseerbits += -offset*width;
					for (auto bit : sigmap(cell->getPort(ID(A))))
						bitoffsets[bit] = std::max(bitoffsets[bit], -offset);
				}
			}

			for (auto pair : bitoffsets)
				nunique_seerbits += pair.second;

			for (auto cell : m->selected_cells())
				db.register_cell(cell);
			db.traverse_sources();

			bitoffsets.clear();

			for (auto cell : m->selected_cells())
			for (auto &conn : cell->connections()) {
				if (ct.cell_output(cell->type, conn.first))
					continue;

				if (cell->type == ID($mux) &&
					conn.first == ID::S &&
					(cell->getPort(ID::A).is_fully_undef() ||
					 cell->getPort(ID::B).is_fully_undef()))
					continue;

				for (auto bit : sigmap(conn.second)) {
					auto upstream = db.lookup(bit);
					bitoffsets[upstream.bit] = std::max(bitoffsets[upstream.bit], -upstream.offset);

					if (upstream.offset > 0)
						found_magic2 = true;
				}
			}

			for (auto port : m->ports)
			for (auto bit : sigmap(m->wire(port))) {
				auto upstream = db.lookup(bit);
				bitoffsets[upstream.bit] = std::max(bitoffsets[upstream.bit],
														-upstream.offset);
			}

			for (auto pair : bitoffsets)
				nunique_seerbits2 += pair.second;
		}

		log("   Number of seers:               %6d\n", nseers);
		log("   Number of magic seers:         %6d\n", nmagic_seers);
		log("\n");
		log("   -- Seer bits --\n");
		log("   Sum:     %6d\n", nseerbits);
		log("   Unique:  %6d\n", nunique_seerbits);
		log("   Unique2: %6d\n", nunique_seerbits2);
		log("\n");

		if (assert_max && nunique_seerbits2 > assert_val)
			log_error("Assertion failed (actual %d > asserted %d).\n",
					  nunique_seerbits2, assert_val);

		if (!mask_undef_mux && assert_no_magic && nmagic_seers > 0)
			log_error("Assertion failed (found %d magic seers in design).\n",
					  nmagic_seers);

		if (mask_undef_mux && assert_no_magic && found_magic2)
			log_error("Assertion failed (found magic seers in design).\n");
	}
} SeerStatPass;
