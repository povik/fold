// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include "kernel/cellaigs.h"
#include "kernel/celltypes.h"
#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

void reachablity(CellTypes &ct, Module *m, bool write_attrs, bool connect_const)
{
	SigMap sigmap(m);
	log_assert(sigmap(SigSpec({State::S0, State::S1, State::Sx})) \
					== SigSpec({State::S0, State::S1, State::Sx}));

	struct reachables {
		bool low;
		bool high;

		reachables()
			: low(false), high(false) {}

		State convert() const
		{
			switch (low | (high << 1)) {
			case 0b00: return State::Sx;
			case 0b01: return State::S0;
			case 0b10: return State::S1;
			default:
			case 0b11: return State::Sm;
			}
		}

		void invert()
		{
			bool save;
			save = low;
			low = high;
			high = save;
		}

		bool extends(reachables other) const
		{
			if (other.high && !high) return false;
			if (other.low && !low) return false;
			return true;
		}

		void reset()
		{
			low = high = false;
		}

		void set(State bit)
		{
			reset();
			add(bit);
		}

		bool add(State bit)
		{
			State save = convert();

			switch (bit) {
			case State::Sx: break;
			case State::S0: low = true; break;
			case State::S1: high = true; break;
			case State::Sm: low = high = true; break;
			default: break;
			}

			return save != convert();
		}

		bool add(const reachables &other)
		{
			return add(other.convert());
		}
	};

	dict<SigBit, reachables> state;
	state[State::S0].add(State::S0);
	state[State::S1].add(State::S1);

	for (auto wire : m->wires())
	if (wire->has_attribute(ID(seer.background_value))) {
		Const attr_val = wire->attributes[ID(seer.background_value)];
		SigSpec wiresig = sigmap(wire);
		log_assert(attr_val.size() == wiresig.size());

		for (int i = 0; i < attr_val.size(); i++)
			state[wiresig[i]].add(attr_val[i]);
	}

	for (auto wire : m->wires())
	if (wire->has_attribute(ID(seer.reachable_states))) {
		Const attr_val = wire->attributes[ID(seer.reachable_states)];
		SigSpec wiresig = sigmap(wire);
		log_assert(attr_val.size() == wiresig.size());

		for (int i = 0; i < attr_val.size(); i++)
			state[wiresig[i]].add(attr_val[i]);
	}

	for (auto wire : m->wires())
	if (wire->port_input) {
		SigSpec wiresig = sigmap(wire);

		for (int i = 0; i < wiresig.size(); i++)
			state[wiresig[i]].add(State::Sm);
	}

	dict<Cell *, Aig> cell_aigs;

	int npasses = 0;
	while (true) {
		bool did_something_any_cell = false;
		std::string resolved_by;

		for (auto cell : m->cells()) {
			bool did_something = false;

			if (cell->type.in(ID($ff), ID(SEER), ID(IMPORT))) {
				SigSpec in, out;
				if (cell->type == ID(SEER)) {
					in = sigmap(cell->getPort(ID(A)));
					out = sigmap(cell->getPort(ID(Y)));
				} else {
					in = sigmap(cell->getPort(ID(D)));
					out = sigmap(cell->getPort(ID(Q)));
				}

				log_assert(in.size() == out.size());
				for (int i = 0; i < in.size(); i++) {
					log_assert(state[in[i]].extends(state[out[i]]) || true);
					did_something |= state[out[i]].add(state[in[i]].convert());
				}

				resolved_by = "pass-through memory element";
				goto done;
			}

			if (cell->type.in(ID($mux), ID($pmux))) {
				SigSpec S = sigmap(cell->getPort(ID::S));
				SigSpec A = sigmap(cell->getPort(ID::A));
				SigSpec B = sigmap(cell->getPort(ID::B));
				SigSpec Y = sigmap(cell->getPort(ID::Y));

				resolved_by = "$pmux";
				std::vector<reachables> out_state(Y.size());

				for (int i = 0; i < S.size(); i++) {
					switch (state[S[i]].convert()) {
					case State::Sx:
						// As long as there is at least one assured x-state bit on the select port
						// there can't be any defined output on the mux
						goto done;

					default:
						break;
					}
				}

				for (int j = 0; j < Y.size(); j++)
					out_state[j].add(state[A[j]]);

				for (int i = 0; i < S.size(); i++) {
					switch (state[S[i]].convert()) {
					case State::S0:
						// This case cannot ever be selected
						continue;

					case State::S1:
						// Either this case will always be selected or the mux will
						// output all-xes. Either way we can soundly model the mux
						// with the below
						for (int j = 0; j < Y.size(); j++)
							out_state[j].set(state[B[i * Y.size() + j]].convert());
						goto mux_resolved;

					case State::Sm:
						for (int j = 0; j < Y.size(); j++)
							out_state[j].add(state[B[i * Y.size() + j]]);
						break;

					default:
						break;
					}
				}

			mux_resolved:
				for (int j = 0; j < Y.size(); j++)
					did_something |= state[Y[j]].add(out_state[j]);
				goto done;
			}

			goto aig;
		aig:
			{
				if (!cell_aigs.count(cell))
					cell_aigs.emplace(cell, Aig(cell));
				Aig &aig = cell_aigs.find(cell)->second;

				if (aig.name.empty())
					goto unmodeled;

				//log_debug("Going over AIG %s of cell '%s'\n", log_id(aig.name), log_id(cell->name));

				vector<reachables> aigstates;
				for (int i = 0; i < (int) aig.nodes.size(); i++) {
					AigNode &node = aig.nodes[i];

					reachables out;
					if (!node.portname.empty()) {
						SigBit cell_port = sigmap(cell->getPort(node.portname)[node.portbit]);
						out = state[cell_port];
					} else if (node.left_parent < 0 && node.right_parent < 0) {
						out.add(State::S0);
					} else {
						reachables left = aigstates[node.left_parent];
						reachables right = aigstates[node.right_parent];

						if (left.low || right.low)
							out.add(State::S0);
						if (left.high && right.high)
							out.add(State::S1);
					}

					if (node.inverter)
						out.invert();

					//log_debug("  #%d out:%s port:%s(%d) left:%d right:%d invert:%d\n",
					//		  i, log_signal(out.convert()), log_id(node.portname), node.portbit,
					//		  node.left_parent, node.right_parent, node.inverter);

					for (auto &aport : node.outports) {
						SigBit cell_port = sigmap(cell->getPort(aport.first)[aport.second]);
						//log_debug("    output %s[%d]\n", log_id(aport.first), aport.second);
						did_something |= state[cell_port].add(out.convert());
					}
					aigstates.push_back(out);
				}
				resolved_by = stringf("aig %s", aig.name.c_str());
			}
			goto done;

		unmodeled:
			for (auto &conn : cell->connections()) {
				if (!ct.cell_output(cell->type, conn.first))
					continue;
				for (auto bit : sigmap(conn.second))
					did_something |= state[bit].add(State::Sm);
			}
			resolved_by = "unmodeled";
			goto done;

		done:
			if (!did_something)
				continue;

			log_debug("%s\t%s:\t", log_id(cell->type), log_id(cell->name));
			for (auto &conn : cell->connections()) {
				std::string str;
				for (auto bit : sigmap(conn.second)) {
					switch (state[bit].convert()) {
					case State::S0:
						str += "0";
						break;
					case State::S1:
						str += "1";
						break;
					case State::Sx:
						str += "x";
						break;
					case State::Sm:
						str += "-";
						break;
					default:
						str += " ";
						break;
					}
				}
				std::reverse(str.begin(), str.end());
				log_debug("%s=%s ", log_id(conn.first), str.c_str());
			}
			log_debug("\n");

			did_something_any_cell = true;
			if (write_attrs)
				cell->attributes[ID(seer.reachability_resolution)] = \
																resolved_by;
		}

		npasses++;
		if (!did_something_any_cell)
			break;
	}

	log_debug("Made %d passes over the circuit.\n", npasses);

	if (write_attrs)
	for (auto wire : m->selected_wires()) {
		SigSpec sig = sigmap(wire);
		Const reachable_states(State::Sx, sig.size());
		for (int i = 0; i < sig.size(); i++)
			reachable_states[i] = state[sig[i]].convert();
		wire->attributes[ID(seer.reachable_states)] = reachable_states;
	}

	SigSpec constbits;

	if (connect_const)
	for (auto pair : state)
	if (pair.first.is_wire() && pair.second.convert() != State::Sm)
		constbits.append(pair.first);

	dict<SigBit, SigBit> driver_setaside;

	constbits.sort_and_unify();
	for (auto chunk : constbits.chunks()) {
		SigSpec sig = chunk;
		SigSpec wire = m->addWire(NEW_ID, chunk.size());
		for (int i = 0; i < chunk.size(); i++)
			driver_setaside[sig[i]] = wire[i];
		Const val(State::Sx, chunk.size());
		for (int i = 0; i < chunk.size(); i++)
			val[i] = state[sig[i]].convert();
		log_debug("Connecting %s to %s\n", log_signal(chunk), log_signal(val));
		m->connect(chunk, val);
	}

	for (auto cell : m->cells())
	for (auto &conn : cell->connections_) {
		if (!ct.cell_output(cell->type, conn.first))
			continue;
		sigmap(conn.second).replace(driver_setaside, &conn.second);
	}
}

struct SeerReachabilityPass : Pass {
	SeerReachabilityPass() : Pass("seer_reachability", "do coarse reasoning over the reachable seer circuit states") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_REACHABILITY pass. (explore reachable seer circuit states)\n");

		bool write_attrs = false;
		bool connect_const = false;

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			if (args[argidx] == "-write_attrs")
				write_attrs = true;
			else if (args[argidx] == "-connect_const")
				connect_const = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		CellTypes ct(d);

		for (auto m : d->selected_modules()) {
			if (!d->selected_whole_module(m->name)) {
				log("Skipping partially selected module '%s'.\n", log_id(m->name));
				continue;
			}

			reachablity(ct, m, write_attrs, connect_const);
		}
	}
} SeerReachabilityPass;

PRIVATE_NAMESPACE_END
