// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <assert.h>

#include "kernel/yosys.h"
#include "kernel/sigtools.h"
#include "kernel/celltypes.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

#include "shared.h"
#include "timetravel.h"

using namespace RTLIL;

IdString rename_wire(IdString id, int offset)
{
	auto id_str = id.str();
	log_assert(id_str.size() >= 1);

	std::string basename;
	int old_offset;

	int i;
	for (i = id_str.size()-1; i > 0 && isdigit(id_str[i]); i--);

	if (id_str.substr(std::max(0, i - 5)) == "_delay") {
		basename = id_str.substr(0, std::max(0, i - 6));
		old_offset = -atoi(id_str.c_str() + i + 1);
	} else if (id_str.substr(std::max(0, i - 5)) == "_ahead") {
		basename = id_str.substr(0, std::max(0, i - 6));
		old_offset = atoi(id_str.c_str() + i + 1);
	} else {
		basename = id_str;
		old_offset = 0;
	}

	offset += old_offset;

	if (offset == 0)
		return IdString(basename);
	else if (offset > 0)
		return IdString(basename + "_ahead" + std::to_string(offset));
	else
		return IdString(basename + "_delay" + std::to_string(-offset));
}

CellTypes our_celltypes;

void timetravel(Module* m, CellTypes &ct,
				pool<timetravel_node> all_nodes,
				pool<timetravel_node> nodes, int offset)
{
	SigMap sigmap(m);
	dict<SigBit,State> bgval_attr;

	for (auto wire : m->wires())
	if (wire->has_attribute(ID(seer.background_value))) {
		Const attr_val = wire->attributes[ID(seer.background_value)];
		SigSpec wiresig = sigmap(wire);
		for (int i = 0; i < attr_val.size(); i++) {
			SigBit wirebit = wiresig[i];
			State attrbit = attr_val[i];
			if (attrbit == State::Sx)
				continue;
			bgval_attr[wirebit] = attrbit;
		}
	}

	dict<SigBit, timetravel_node> drivers;
	for (auto node : all_nodes)
	for (auto &conn : node.cell->connections()) {
		if (!ct.cell_output(node.cell->type, conn.first) ||
				node.port_masked(conn.first))
			continue;
		for (auto bit : sigmap(conn.second))
			drivers[bit] = node;
	}

	SigPool will_have_counterpart;
	SigPool driver_traveled;
	SigPool need_fwd_travel;
	SigPool need_back_travel;

	for (auto node : nodes)
	for (auto &conn : node.cell->connections()) {
		if (node.port_masked(conn.first))
			continue;
		for (auto bit : sigmap(conn.second))
			will_have_counterpart.add(bit);
		if (ct.cell_output(node.cell->type, conn.first))
			for (auto bit : sigmap(conn.second))
				driver_traveled.add(bit);
	}

	for (auto node : all_nodes) {
		if (nodes.count(node))
			continue; /* pick non-traveled nodes */

		for (auto &conn : node.cell->connections()) {
			if (node.port_masked(conn.first))
				continue;
			if (!ct.cell_input(node.cell->type, conn.first))
				continue;
			for (auto bit : sigmap(conn.second))
				if (driver_traveled.check(bit))
					need_back_travel.add(bit);
		}
	}
	for (auto wire_id : m->ports) {
		auto w = m->wire(wire_id);
		if (w == nullptr)
			continue;
		for (auto bit : sigmap(w))
			if (driver_traveled.check(bit))
				need_back_travel.add(bit);
	}

	for (auto bit : will_have_counterpart.export_all())
		if (!driver_traveled.check(bit))
			need_fwd_travel.add(bit);

	dict<SigBit, SigBit> bitcparts;

	for (auto b : { S0, S1, Sx, Sz, Sa })
		bitcparts[b] = b;

	// do not travel the clock wrt which we are doing
	// the time traveling
	auto clk = m->wire(ID(clk));
	if (clk) {
		for (auto bit : sigmap(clk)) {
			need_fwd_travel.del(bit);
			will_have_counterpart.del(bit);
			bitcparts[bit] = bit;
		}
	}

	SigSpec will_have_counterpart_ = will_have_counterpart.export_all();
	will_have_counterpart_.sort_and_unify();
	for (auto chunk : will_have_counterpart_.chunks()) {
		auto new_name = chunk.is_wire()
			? rename_wire(chunk.wire->name, offset) \
			: NEW_ID;
		while (new_name != NEW_ID && m->wire(new_name) != NULL)
			new_name = IdString(new_name.str() + "_");
		Wire *cpart_wire = m->addWire(new_name, chunk.width);
		SigSpec cpart = sigmap(cpart_wire);

		bool nonempty = false;
		Const attr_val(State::Sx, chunk.width);
		for (int i = 0; i < chunk.width; i++) {
			bitcparts[SigBit(chunk, i)] = cpart[i];
			if (bgval_attr.count(SigBit(chunk, i))) {
				attr_val[i] = bgval_attr[SigBit(chunk, i)];
				nonempty = true;
			}
		}
		if (nonempty)
			cpart_wire->attributes[ID(seer.background_value)] = attr_val;
	}

	for (auto node : nodes)
	for (auto &conn : node.cell->connections()) {
		if (node.port_masked(conn.first))
			continue;
		std::vector<SigBit> newbits;
		for (auto bit : sigmap(conn.second))
			newbits.push_back(bitcparts[bit]);
		node.cell->setPort(conn.first, SigSpec(newbits));
	}

	SigSpec need_fwd_travel_ = need_fwd_travel.export_all();
	need_fwd_travel_.sort_and_unify();
	for (auto chunk : need_fwd_travel_.chunks()) {
		SigSpec spec = chunk;
		std::vector<SigBit> newbits;
		for (auto bit : spec)
			newbits.push_back(bitcparts[bit]);
		SigSpec cpartspec = SigSpec(newbits);

		create_seer(m, NEW_ID, spec, cpartspec, offset);
	}
	SigSpec need_back_travel_ = need_back_travel.export_all();
	need_back_travel_.sort_and_unify();
	for (auto chunk : need_back_travel_.chunks()) {
		SigSpec spec = chunk;
		std::vector<SigBit> newbits;
		for (auto bit : spec)
			newbits.push_back(bitcparts[bit]);
		SigSpec cpartspec = SigSpec(newbits);

		create_seer(m, NEW_ID, cpartspec, spec, -offset);
	}
}

struct SeerTimetravelPass : Pass {
	SeerTimetravelPass() : Pass("seer_timetravel", "perform time travel of circuit nodes") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_TIMETRAVEL pass. (time travel circuit nodes)\n");

		bool keep_attribute = false;

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-keep-attr")
				keep_attribute = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		int offset = 0;

		CellTypes ct;
		ct.setup(d);

		for (auto m : d->selected_modules()) {
			pool<timetravel_node> selected_nodes;

			for (auto cell : m->selected_cells()) {
				if (!ct.cell_known(cell->type))
					log_error("Unknown cell encountered: %s (%s)\n",
							log_id(cell->name), log_id(cell->type));

				if (!cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE))) {
					selected_nodes.insert(timetravel_node(cell));
				} else {
					for (auto bank : { ID::A, ID::B }) {
						auto node = timetravel_node::with_bank(cell, bank);
						selected_nodes.insert(node);
					}
				}
			}

			if (offset) {
				pool<timetravel_node> all_nodes;
				for (auto cell : m->cells()) {
					if (!ct.cell_known(cell->type))
						log_error("Unknown cell encountered: %s (%s)\n",
								log_id(cell->name), log_id(cell->type));

					if (cell->type != ID(TIMEPORTAL)) {
						all_nodes.insert(timetravel_node(cell));
					} else {
						for (auto bank : { ID::A, ID::B }) {
							auto node = timetravel_node::with_bank(cell, bank);
							all_nodes.insert(node);
						}
					}
				}
				timetravel(m, ct, all_nodes, selected_nodes, offset);
			} else {
				dict<timetravel_node, int> offsets;
				for (auto node : selected_nodes)
					offsets[node] = -node.get_timetravel_attribute();
				dict<int,pool<timetravel_node>> offset_nodes;
				for (auto pair : offsets) {
					if (!pair.second)
						continue;
					offset_nodes[pair.second].insert(pair.first);
				}
				for (auto pair : offset_nodes) {
					log("Time traveling %ld nodes by offset %d\n",
						pair.second.size(), pair.first);
					for (auto cell : pair.second)
						log_debug("\t%s\n", log_id(cell.name()));
					pool<timetravel_node> all_nodes;
					for (auto cell : m->cells()) {
						if (!ct.cell_known(cell->type))
							log_error("Unknown cell encountered: %s (%s)\n",
									log_id(cell->name), log_id(cell->type));

						if (!cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE))) {
							all_nodes.insert(timetravel_node(cell));
						} else {
							for (auto bank : { ID::A, ID::B }) {
								auto node = timetravel_node::with_bank(cell, bank);
								all_nodes.insert(node);
							}
						}
					}
					timetravel(m, ct, all_nodes, pair.second, pair.first);
				}

				if (!offset_nodes.empty())
					d->scratchpad_set_bool("seer.did_something", true);
				if (!keep_attribute) {
					for (auto node : selected_nodes)
						node.unset_timetravel_attribute();
				}
			}
		}
	}
} SeerTimetravelPass;

PRIVATE_NAMESPACE_END
