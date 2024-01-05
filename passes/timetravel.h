// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#ifndef __TIMETRAVEL_H
#define __TIMETRAVEL_H

#include "kernel/rtlil.h"
#include <assert.h>

struct timetravel_node {
	Cell *cell;

	/* timeportal/backedge bank: A for A->BY path, B for B->AY path */
	IdString bank;

	timetravel_node(Cell *cell)
			: cell(cell), bank() {
		assert(!cell || !cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE)));
	}

	static timetravel_node perimeter;

	static timetravel_node with_bank(Cell *cell, IdString bank) {
		assert(cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE)));
		assert(bank == ID::A || bank == ID::B);
		return timetravel_node(cell, bank);
	}
	timetravel_node partner() {
		assert(cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE)));
		return with_bank(cell, bank == ID::A ? ID::B : ID::A);
	}
	bool is_perimeter() const {
		return !cell;
	}
	bool is_timeportal() const {
		return cell && cell->type == ID(TIMEPORTAL);
	}
	bool is_backedge() const {
		return cell && cell->type == ID(BACKEDGE);
	}
	bool is_seer() const {
		return cell && cell->type == ID(SEER);
	}
	int seer_offset() {
		assert(is_seer());
		return cell->getParam(ID(OFFSET)).as_int(true);
	}
	bool operator==(const timetravel_node &other) const {
		return this->cell == other.cell && this->bank == other.bank;
	}
	bool operator!=(const timetravel_node &other) const {
		return !(*this == other);
	}
	unsigned int hash() const {
		return (cell ? cell->hash() : 0) + (bank == ID::A);
	}
	bool port_masked(IdString id) const {
		if (id == ID(CLK) || id == ID(TRG))
			return true;
		if (!is_timeportal() && !is_backedge())
			return false;
		if (id == ID(BY))
			id = ID::A;
		if (id == ID(AY))
			id = ID::B;
		return id != bank;
	}
	IdString attrname_domain() {
		assert(is_timeportal());
		if (bank == ID::A)
			return ID(timetravel_bank_a_domain);
		else
			return ID(timetravel_bank_b_domain);
	}
	IdString attrname_constraint() {
		assert(is_timeportal());
		if (bank == ID::A)
			return ID(timetravel_bank_a_constraint);
		else
			return ID(timetravel_bank_b_constraint);
	}
	IdString attrname_timetravel() {
		if (!is_timeportal() && !is_backedge())
			return ID(timetravel);
		else if (bank == ID::A)
			return ID(timetravel_bank_a);
		else
			return ID(timetravel_bank_b);
	}
	IdString attrname_fixed() {
		if (!is_timeportal())
			return ID(timetravel_fixed);
		else if (bank == ID::A)
			return ID(timetravel_bank_a_fixed);
		else
			return ID(timetravel_bank_b_fixed);
	}
	IdString timetravel_domain() {
		assert(is_timeportal());
		if (!cell->has_attribute(attrname_domain()))
			return IdString(cell->name.str() + "_" + bank.str());
		return RTLIL::escape_id(
			cell->get_string_attribute(attrname_domain()));
	}
	bool domain_constrained_from_below() {
		if (!cell->has_attribute(attrname_constraint()))
			return false;
		auto constraint = cell->get_string_attribute(attrname_constraint());
		if (constraint == "lower-bound")
			return true;
		if (constraint == "fixed")
			return true;
		return false;
	}
	bool domain_constrained_from_above() {
		if (!cell->has_attribute(attrname_constraint()))
			return false;
		auto constraint = cell->get_string_attribute(attrname_constraint());
		if (constraint == "upper-bound")
			return true;
		if (constraint == "fixed")
			return true;
		return false;
	}
	bool fixed() {
		if (!cell)
			return true;
		return cell->get_bool_attribute(attrname_fixed());
	}
	IdString name() {
		if (cell)
			return cell->name;
		else
			return ID(perimeter);
	}
	void set_timetravel_attribute(int v) {
		if (!cell) {
			assert(v == 0);
			return;
		}
		cell->set_string_attribute(attrname_timetravel(), std::to_string(v));
	}
	int get_timetravel_attribute() {
		auto attrname = attrname_timetravel();
		if (!cell || !cell->has_attribute(attrname))
			return 0;
		auto attrval = cell->get_string_attribute(attrname);
		try {
			return std::stoi(attrval);
		} catch (std::invalid_argument const&) {
			log_error("Bad %s attribute on %s: %s\n",
						log_id(attrname), log_id(name()), attrval.c_str());
		}
	}
	void unset_timetravel_attribute() {
		if (!cell)
			return;
		cell->attributes.erase(attrname_timetravel());
	}
	timetravel_node()
		: cell(NULL), bank()
	{}
private:
	timetravel_node(Cell *cell, IdString bank)
		: cell(cell), bank(bank)
	{}
};

timetravel_node timetravel_node::perimeter(NULL);

static bool is_undef_mux(Cell *cell)
{
	return cell->type == ID($mux) &&
		(cell->getPort(ID::A).is_fully_undef() ||
		 cell->getPort(ID::B).is_fully_undef());
}

struct timetravel_module {
	Module *m;
	SigMap &sigmap;
	CellTypes &ct;
	bool mask_undef_mux;

	dict<SigBit, timetravel_node> drivers;
	pool<timetravel_node> nodes;
	//pool<timetravel_node> selected_nodes;

	void register_node(timetravel_node node)
	{
		nodes.insert(node);
		for (auto bit : output_bits(node)) {
			if (!bit.is_wire()) continue;
			assert(!drivers.count(bit));
			drivers[bit] = node;
		}
	}

	timetravel_module(Module *m, SigMap& sigmap, CellTypes &ct, bool mask_undef_mux=false)
		: m(m), sigmap(sigmap), ct(ct), mask_undef_mux(mask_undef_mux)
	{
		for (auto cell : m->cells()) {
			if (!cell->type.in(ID(TIMEPORTAL), ID(BACKEDGE))) {
				register_node(timetravel_node(cell));
			} else {
				for (auto bank : { ID::A, ID::B })
					register_node(
						timetravel_node::with_bank(cell, bank));
			}
		}

		/*
		 * Now when drivers for all nodes but the perimeter
		 * are in, output_bits() can correctly decide which
		 * port bits are inputs and which outputs.
		 */

		register_node(timetravel_node::perimeter);
	}

	struct portbit {
		IdString label;
		int offset;
	};

	// TODO: merge with input_bits
	void visit_inputs(timetravel_node node,
					  std::function<void(SigBit bit, portbit port)> f) {
		if (node.is_perimeter()) {
			for (auto port : m->ports) {
				int nbit = 0;
				for (auto bit : sigmap(m->wire(port))) {
					if (drivers.count(bit) && drivers[bit] != node)
						f(bit, portbit{ port, nbit });
					nbit++;
				}
			}
		} else {
			assert(node.cell && ct.cell_known(node.cell->type));
			for (auto conn : node.cell->connections()) {
				if (ct.cell_output(node.cell->type, conn.first) ||
						node.port_masked(conn.first))
					continue;
				if (mask_undef_mux && is_undef_mux(node.cell) && conn.first == ID::S)
					continue;
				int nbit = 0;
				for (auto bit : sigmap(conn.second))
					f(bit, portbit{ conn.first, nbit++ });
			}
		}
	}

	SigSpec input_bits(timetravel_node &node) {
		SigSpec ret;

		if (node.is_perimeter()) {
			for (auto port : m->ports)
			for (auto bit : sigmap(m->wire(port)))
				if (drivers.count(bit) && drivers[bit] != node)
					ret.append(bit);
		} else {
			assert(node.cell && ct.cell_known(node.cell->type));
			for (auto conn : node.cell->connections()) {
				if (ct.cell_output(node.cell->type, conn.first) ||
						node.port_masked(conn.first))
					continue;
				if (mask_undef_mux && is_undef_mux(node.cell) && conn.first == ID::S)
					continue;
				for (auto bit : sigmap(conn.second))
					ret.append(bit);
			}
		}

		ret.sort_and_unify();
		return ret;
	}

	// TODO: merge with output_bits
	void visit_outputs(timetravel_node node,
						  std::function<void(SigBit bit, portbit port)> f) {
		if (node.is_perimeter()) {
			for (auto port : m->ports) {
				int nbit = 0;
				for (auto bit : sigmap(m->wire(port))) {
					if (!drivers.count(bit) || drivers[bit] == node)
						f(bit, portbit{port, nbit});
					nbit++;
				}
			}
		} else {
			assert(node.cell && ct.cell_known(node.cell->type));
			for (auto conn : node.cell->connections()) {
				if (!ct.cell_output(node.cell->type, conn.first) ||
						node.port_masked(conn.first))
					continue;
				int nbit = 0;
				for (auto bit : sigmap(conn.second))
					f(bit, portbit{conn.first, nbit++});
			}
		}
	}

	SigSpec output_bits(timetravel_node &node) {
		SigSpec ret;

		if (node.is_perimeter()) {
			for (auto port : m->ports)
			for (auto bit : sigmap(m->wire(port)))
				if (!drivers.count(bit) || drivers[bit] == node)
					ret.append(bit);
		} else {
			assert(node.cell && ct.cell_known(node.cell->type));
			for (auto conn : node.cell->connections()) {
				if (!ct.cell_output(node.cell->type, conn.first) ||
						node.port_masked(conn.first))
					continue;
				for (auto bit : sigmap(conn.second))
					ret.append(bit);
			}
		}

		ret.sort_and_unify();
		return ret;
	}
};

#endif /* __TIMETRAVEL_H */
