// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <vector>

#include "kernel/sigtools.h"
#include "kernel/celltypes.h"

#include "coroutine.h"

USING_YOSYS_NAMESPACE

//FIXME: not in private namespace because of hashlib specialization
//PRIVATE_NAMESPACE_BEGIN

#include "shared.h"
#include "timetravel.h"

using hashlib::hash_ops;
using hashlib::hash_cstr_ops;
using hashlib::hash_ptr_ops;
using hashlib::hash_obj_ops;

namespace resolve {

struct node;
struct edge {
	node *base;
	node *endpoint;
	int cost;
	int offset;

	bool is_saturated();
	bool check();

	/* scratchpad for passes (lagrange multiplier) */
	int lagr;

	void add_lagr(int delta);
	void reset_lagr();
};

typedef node* rnode;
typedef edge redge;

};

template<> struct Yosys::hashlib::hash_ops<resolve::rnode> : ::Yosys::hashlib::hash_ptr_ops {};

namespace resolve {

class edge_iterator {
protected:
	bool use_it2;
	std::vector<edge>::iterator it1;
	std::vector<std::reference_wrapper<edge>>::iterator it2;
public:
	using iterator_category = std::input_iterator_tag;
	using value_type = edge;
	using difference_type = ptrdiff_t;
	using pointer = edge*;
	using reference = edge&;

	edge_iterator(std::vector<edge>::iterator it1) : use_it2(false), it1(it1) {}
	edge_iterator(std::vector<std::reference_wrapper<edge>>::iterator it2) : use_it2(true), it2(it2) {}
	edge_iterator& operator++() { if (use_it2) it2++; else it1++; return *this; }
	bool operator==(edge_iterator other) const {
		return use_it2 == other.use_it2 && (use_it2 ? it2 == other.it2 : it1 == other.it1);
	}
	bool operator!=(edge_iterator other) const { return !(*this == other); }
	reference operator*() const { if (use_it2) return *it2; else return *it1; }
};

struct edge_wrapper {
	bool pick_redges;
	std::vector<edge> &edges;
	std::vector<std::reference_wrapper<edge>> &redges;

	edge_iterator begin() {
		if (!pick_redges)
			return edge_iterator(edges.begin());
		else
			return edge_iterator(redges.begin());
	}

	edge_iterator end() {
		if (!pick_redges)
			return edge_iterator(edges.end());
		else
			return edge_iterator(redges.end());
	}

	edge_wrapper(std::vector<edge> &edges,
			std::vector<std::reference_wrapper<edge>> &redges,
			bool pick_redges)
		: pick_redges(pick_redges), edges(edges), redges(redges) {}
};

struct node {
	IdString label;
	std::vector<edge> _edges;
	int assign;

	/* working scratchpad */
	std::vector<std::reference_wrapper<edge>> _redges;
	int pressure;
	int push;

	edge_wrapper edges;
	edge_wrapper redges;

	node()
		: edges(_edges, _redges, false), redges(_edges, _redges, true) {}

	int normalize_edges()
	{
		int lapse = 0;

		dict<node*,edge> map;
		for (auto edge : _edges) {
			if (!map.count(edge.endpoint)) {
				map[edge.endpoint] = edge;
			} else {
				auto &map_edge = map[edge.endpoint];
				lapse -= map_edge.offset * map_edge.cost;
				map_edge.offset = std::max(map_edge.offset, edge.offset);
				map_edge.cost += edge.cost;
				lapse += map_edge.offset * map_edge.cost;
				lapse -= edge.cost * edge.offset;
			}
		}
		_edges.clear();
		for (auto pair : map) {
			if (pair.first == this) {
				assert(pair.second.offset <= 0);
				continue;
			}
			_edges.push_back(pair.second);
		}

		return lapse;
	}
};

bool edge::is_saturated()
{
	return (endpoint->assign + offset) == base->assign;
}

bool edge::check()
{
#if 0
	log_debug("EDGE CHECK: (%s) %d + %d <= (%s) %d\n",
			  log_id(endpoint->label), endpoint->assign,
			  offset, log_id(base->label), base->assign);
#endif
	return (endpoint->assign + offset) <= base->assign;
}

void edge::add_lagr(int delta)
{
	lagr += delta;
	base->pressure += delta;
	endpoint->pressure -= delta;
	assert(lagr >= 0);
}

void edge::reset_lagr()
{
	add_lagr(-lagr);
}

struct resolvnet {
	pool<node*> nodes;

	struct fix {
		node *upstream;
		/* the fixed node's assign will be upstream->assign + offset */
		int offset;
	};
	dict<node*,fix> fixed;

	int costlapse = 0;

	node *add_node()
	{
		node *n = new node;
		nodes.insert(n);
		return n;
	}

	fix look_up_fix(rnode node)
	{
		int walk_offset = 0;
		while (fixed.count(node)) {
			auto link = fixed[node];
			node = link.upstream;
			walk_offset += link.offset;
		}

		return fix{
			node,
			walk_offset
		};
	}

	void set_fix(rnode from, fix target)
	{
		assert(!fixed.count(target.upstream));

		fix prior = look_up_fix(from);

		if (prior.upstream == target.upstream) {
			assert(prior.offset == target.offset);
			return;
		}

		fixed[prior.upstream] = fix{
			target.upstream,
			target.offset - prior.offset
		};
	}

	void add_edge(node *from, node *to, int cost=0, int offset=0)
	{
		if (from == to) {
			if (offset > 0) {
				log_error("Bad edge: from %s to itself with offset %d.\n",
						  log_id(from->label), offset);
			}
			assert(offset <= 0);
			return;
		}

		for (auto &edge : from->_edges) {
			if (edge.endpoint == to) {
				edge.cost += cost;
				edge.offset = std::max(edge.offset, offset);
				return;
			}
		}

		from->_edges.push_back(edge{
			.base = from,
			.endpoint = to,
			.cost = cost,
			.offset = offset,
			.lagr = 0
		});
	}
};

struct merge_saturated_components {
	resolvnet &rnet;

	struct indices {
		int index;
		int lowlink;
	};

	pool<rnode> on_stack;
	dict<rnode, indices> labels;
	std::vector<rnode> stack;

	int index = 0;
	int ndomains = 0;

	merge_saturated_components(resolvnet &rnet)
		: rnet(rnet) {}

	void descend(rnode node)
	{
		labels[node] = indices{ index, index };
		index++;

		stack.push_back(node);
		on_stack.insert(node);

		for (auto &edge : node->edges) {
			if (!edge.is_saturated())
				continue;

			if (!labels.count(edge.endpoint)) {
				descend(edge.endpoint);
				labels[node].lowlink = std::min(labels[node].lowlink, labels[edge.endpoint].lowlink);
			} else if (on_stack.count(edge.endpoint)) {
				labels[node].lowlink = std::min(labels[node].lowlink, labels[edge.endpoint].index);
			}
		}

		if (labels[node].lowlink == labels[node].index) {
			auto root = std::find(stack.rbegin(), stack.rend(), node);
			assert(root != stack.rend());
			root++;
			emit_component(stack.rbegin(), root);
			for (auto node = stack.rbegin(); node != root; node++)
				on_stack.erase(*node);
			stack.erase(stack.end() - (root - stack.rbegin()), stack.end());
		}
	}

	typedef std::vector<rnode>::reverse_iterator node_iterator;

	void emit_component(node_iterator begin, node_iterator end)
	{
		if (end - begin <= 1)
			return;

		int pivot = (*begin)->assign;
		auto newnode = rnet.add_node();
		newnode->assign = pivot;
		newnode->label = "\\merged_domain" + std::to_string(ndomains);
		ndomains++;

		for (auto it = begin; it != end; it++)
			rnet.set_fix(*it, resolvnet::fix{ newnode, (*it)->assign - pivot });

		log("Merged %ld nodes into '%s'.\n",
			end - begin, log_id(newnode->label));
	}

	void run()
	{
		for (auto node : rnet.nodes) {
			if (labels.count(node))
				continue;
			descend(node);
		}

		for (auto node : rnet.nodes) {
			for (auto& edge : node->edges) {
				if (!rnet.fixed.count(edge.endpoint))
					continue;

				auto endpoint_mb = rnet.look_up_fix(edge.endpoint);
				edge.offset += endpoint_mb.offset;
				edge.endpoint = endpoint_mb.upstream;
			}

			if (!rnet.fixed.count(node))
				continue;

			auto mb = rnet.look_up_fix(node);
			for (auto edge : node->edges)
				rnet.add_edge(mb.upstream, edge.endpoint,
							  edge.cost, edge.offset - mb.offset);
		}

		for (auto pair : rnet.fixed) {
			if (!rnet.nodes.count(pair.first))
				continue;
			rnet.nodes.erase(pair.first);
		}

		for (auto node : rnet.nodes)
			rnet.costlapse += node->normalize_edges();
	}
};

void reset(resolvnet &rnet)
{
	for (auto node : rnet.nodes) {
		node->pressure = 0;
		node->_redges.clear();
	}

	for (auto node : rnet.nodes)
	for (auto& edge : node->edges) {
		edge.lagr = 0;
		edge.endpoint->pressure += edge.cost;
		node->pressure -= edge.cost;
		edge.endpoint->_redges.push_back(edge);
	}
}

class pressure_suction : public coroutine {
public:
	std::vector<rnode> stack;
	pool<rnode> on_stack;

	pool<rnode> tied;
	std::vector<rnode> tied_stack;

	bool early_exit = false;

	volatile bool pol; /* whether we are accumulating positive pressure */
	volatile rnode start_node;

	volatile int offer_avail = 0;
	volatile int offer_taken = 0;

	pressure_suction(coroutine_stack &stack)
		: coroutine(stack) {}

	int offer(int avail) {
		assert(avail >= 0);
		if (avail == 0)
			return 0;
		offer_avail = avail;
		yield();
		return offer_taken;
	}

	int descend(rnode node, int limit) {
		assert(node->push == (pol ? 1 : -1));
		assert(limit >= 0);

		if (on_stack.count(node) || tied.count(node))
			return 0;
		on_stack.insert(node);
		stack.push_back(node);

		tied.insert(node);
		tied_stack.push_back(node);

		int sum_charge = 0;

		int avail = std::min(node->pressure * (pol ? 1 : -1), limit);
		int taken = offer(avail);
		sum_charge += taken;
		limit -= taken;
		if (taken < avail)
			early_exit = true;
		if (!limit || early_exit)
			goto bail;

		for (auto &edge : pol ? node->edges : node->redges) {
			if (!edge.is_saturated())
				continue;
			rnode other = pol ? edge.endpoint : edge.base;
			if (other->push != (pol ? 1 : -1))
				continue;

			int charge = descend(other, limit);
			edge.add_lagr(charge);

			sum_charge += charge;
			limit -= charge;
			if (!limit || early_exit)
				goto bail;
		}

		for (auto &edge : pol ? node->redges : node->edges) {
			if (!edge.is_saturated() || edge.lagr == 0)
				continue;
			rnode other = pol ? edge.base : edge.endpoint;
			if (other->push != (pol ? 1 : -1))
				continue;

			int edge_limit = edge.lagr;
			int tied_save = tied_stack.size();
			int charge = descend(other, std::min(limit, edge_limit));
			edge.add_lagr(-charge);

			if (edge.lagr == 0) {
				auto& stack = tied_stack;

				for (auto it = stack.begin() + tied_save; it != stack.end(); it++)
					tied.erase(*it);
				stack.erase(stack.begin() + tied_save, stack.end());
			}

			sum_charge += charge;
			limit -= charge;
			if (!limit || early_exit)
				goto bail;
		}

bail:
		stack.pop_back();
		on_stack.erase(node);
		return sum_charge;
	}

	void entry() {
		descend(start_node, std::numeric_limits<int>::max());
	}
};

void check_resolution(resolvnet &rnet) {
	for (auto node : rnet.nodes)
		assert(node->push * node->pressure >= 0);

	for (auto node : rnet.nodes)
	for (auto &edge : node->edges) {
		assert(rnet.nodes.count(edge.base) && rnet.nodes.count(edge.endpoint));
		assert((edge.lagr == 0) || ((edge.endpoint->push == edge.base->push) && edge.is_saturated()));
		assert((edge.endpoint->push <= edge.base->push) || !edge.is_saturated());
	}
}

int move(resolvnet &rnet, int dir);

size_t resolve(resolvnet &rnet, bool first) {
	coroutine_stack stack_bottom;
	coroutine_stack stack_top;

	if (first) {
		reset(rnet);
		for (auto node : rnet.nodes) {
			if (node->pressure > 0)
				node->push = 1;
			else
				node->push = -1;
		}
	}

	std::vector<std::reference_wrapper<edge>> conflict_edges;
	for (auto node : rnet.nodes)
	for (auto& edge : node->edges) {
		if (!edge.is_saturated())
			continue;
		assert(rnet.nodes.count(edge.base) && rnet.nodes.count(edge.endpoint));
		if (edge.endpoint->push > edge.base->push)
			conflict_edges.push_back(edge);
	}

	while (!conflict_edges.empty()) {
		auto& edge = conflict_edges.back().get();
		conflict_edges.pop_back();

		if (edge.endpoint->push <= edge.base->push)
			continue;

		pressure_suction bottom(stack_bottom);
		bottom.pol = true;
		bottom.start_node = edge.endpoint;
		bottom.start();

		pressure_suction top(stack_top);
		top.pol = false;
		top.start_node = edge.base;
		top.start();

		int tally = (bottom.running ? bottom.offer_avail : 0) - \
					(top.running ? top.offer_avail : 0);

		while (top.running && bottom.running) {
			assert(tally <= bottom.offer_avail && tally >= -top.offer_avail);

			if (tally > 0) {
				top.offer_taken = top.offer_avail;
				top.resume();
				if (top.running)
					tally -= top.offer_avail;
			} else {
				bottom.offer_taken = bottom.offer_avail;
				bottom.resume();
				if (bottom.running)
					tally += bottom.offer_avail;
			}
		}

		if (top.running) {
			top.offer_taken = top.offer_avail + tally;
			top.resume();
			if (top.running) {
				top.offer_taken = 0;
				top.resume();
			}
		} else if (bottom.running) {
			bottom.offer_taken = bottom.offer_avail - tally;
			bottom.resume();
			if (bottom.running) {
				bottom.offer_taken = 0;
				bottom.resume();
			}
		}
		assert(!top.running && !bottom.running);

		edge.add_lagr(std::min(edge.endpoint->pressure, -edge.base->pressure));

		for (auto node : ((tally > 0) ? top.tied : bottom.tied)) {
			node->push = (tally > 0) ? 1 : -1;
			assert(node->push * node->pressure >= 0);
		}

		assert(edge.endpoint->push <= edge.base->push);
	}

	check_resolution(rnet);

	return conflict_edges.size();
}

int costsum(resolvnet &rnet) {
	int sum = 0;
	for (auto node : rnet.nodes)
	for (auto& edge : node->edges)
		sum += (edge.base->assign - edge.endpoint->assign - edge.offset) * edge.cost;
	return sum + rnet.costlapse;
}

int push(resolvnet &rnet, int dir) {
	int maxmove = std::numeric_limits<int>::max();
	pool<rnode> to_move;

	for (auto node : rnet.nodes)
	for (auto& edge : node->edges) {
		if (edge.endpoint->push > edge.base->push)
			maxmove = std::min(maxmove, edge.base->assign - edge.endpoint->assign - edge.offset);
	}

	int pressuresum = 0;

	for (auto node : rnet.nodes)
	if (node->push == dir) {
		to_move.insert(node);
		pressuresum += node->pressure * dir;
	}

	if (!to_move.empty())
		log("Cost sum: %7d Pressure: %5d Nodes to move: %5zu Distance: %5d \n",
			costsum(rnet), pressuresum, to_move.size(), maxmove);

	for (auto node : to_move)
		node->assign += maxmove * dir;

	return to_move.size();
}

struct order_sccs_worker {
	resolvnet &rnet;

	struct indices {
		int index;
		int lowlink;
	};

	/* one node is special-cased */
	rnode pivot;

	pool<rnode> on_stack;
	dict<rnode, indices> labels;
	std::vector<rnode> stack;

	dict<rnode,int> node_memberships;
	std::vector<pool<rnode>> scc_list;
	std::vector<pool<rnode>> ordered_scc_list;

	int index = 0;
	int sccs = 0;

	order_sccs_worker(resolvnet &rnet, rnode pivot)
		: rnet(rnet), pivot(pivot) {}

	void descend(rnode node)
	{
		labels[node] = indices{ index, index };
		index++;

		if (node == pivot)
			return;

		stack.push_back(node);
		on_stack.insert(node);

		for (auto &edge : node->edges) {
			if (!labels.count(edge.endpoint)) {
				descend(edge.endpoint);
				labels[node].lowlink = std::min(labels[node].lowlink, labels[edge.endpoint].lowlink);
			} else if (on_stack.count(edge.endpoint)) {
				labels[node].lowlink = std::min(labels[node].lowlink, labels[edge.endpoint].index);
			}
		}

		if (labels[node].lowlink == labels[node].index) {
			auto root = std::find(stack.rbegin(), stack.rend(), node);
			assert(root != stack.rend());
			root++;
			emit_component(stack.rbegin(), root);
			for (auto node = stack.rbegin(); node != root; node++)
				on_stack.erase(*node);
			stack.erase(stack.end() - (root - stack.rbegin()), stack.end());
		}
	}

	typedef std::vector<rnode>::reverse_iterator node_iterator;

	void emit_component(node_iterator begin, node_iterator end)
	{
		pool<rnode> scc;
		for (auto it = begin; it != end; it++) {
			scc.insert(*it);
			node_memberships[*it] = scc_list.size();
		}

		scc_list.push_back(scc);

		if (end - begin <= 1)
			return;
	}

	void run()
	{
		for (auto node : rnet.nodes) {
			if (labels.count(node))
				continue;
			descend(node);
		}

		dict<int,pool<int>> edges;
		for (auto it = scc_list.begin(); it != scc_list.end(); it++) {
			auto &scc = *it;
			for (auto node : scc)
			for (auto &edge : node->edges) {
				int our_idx = it - scc_list.begin();
				int other_scc_idx = node_memberships[edge.endpoint];

				if (other_scc_idx != our_idx)
					edges[other_scc_idx].insert(our_idx);
			}
		}

		std::vector<int> nincoming(scc_list.size());
		for (int i = 0; i < (int) scc_list.size(); i++)
		for (auto endpoint : edges[i])
			nincoming[endpoint]++;

		std::vector<int> ordered;
		for (int i = 0; i < (int) scc_list.size(); i++)
			if (!nincoming[i])
				ordered.push_back(i);

		for (int k = 0; k < (int) scc_list.size(); k++) {
			assert(k < (int) ordered.size());
			for (auto endpoint : edges[ordered[k]])
				if (!(--nincoming[endpoint]))
					ordered.push_back(endpoint);
		}

		for (int i = 0; i < (int) scc_list.size(); i++)
			ordered_scc_list.push_back(scc_list[ordered[i]]);
	}
};

std::vector<pool<rnode>> order_sccs(resolvnet &rnet, rnode pivot)
{
	order_sccs_worker worker(rnet, pivot);
	worker.run();
	return worker.ordered_scc_list;
}

struct optimize_worker {
	resolvnet rnet;
	CellTypes ct;
	SigMap sigmap;

	timetravel_module ttm;

	dict<std::pair<timetravel_node, int>,rnode> from_tt_nodes;

	dict<IdString,rnode> domain_nodes;

	rnode rnet_node(timetravel_node tt_node)
	{
		if (!from_tt_nodes.count(std::make_pair(tt_node, 0))) {
			auto node = rnet.add_node();
			from_tt_nodes[std::make_pair(tt_node, 0)] = node;
			node->label = tt_node.name();
			if (tt_node.cell)
				node->assign = tt_node.get_timetravel_attribute();
			else
				node->assign = 0;
			return node;
		}
		return from_tt_nodes[std::make_pair(tt_node, 0)];
	}

	rnode rnet_domain_node(IdString label)
	{
		if (!domain_nodes.count(label)) {
			auto node = rnet.add_node();
			node->label = label;
			domain_nodes[label] = node;
			// Assign of domain nodes gets set later
			return node;
		}
		return domain_nodes[label];
	}

	void assign_minimum(rnode node)
	{
		int max = std::numeric_limits<int>::min();
		for (auto& edge : node->edges)
			max = std::max(max, edge.endpoint->assign + edge.offset);
		node->assign = max;
	}

	optimize_worker(Design *d, Module *m, bool mask_undef_mux)
		: ct(d), sigmap(m), ttm(m, sigmap, ct, mask_undef_mux)
	{
		dict<SigBit,pool<timetravel_node>> consumers_sans_seers;
		dict<SigBit,pool<timetravel_node>> consumers;

		seer_trees_database_t db(sigmap);
		for (auto cell : m->cells())
			db.register_cell(cell);
		db.traverse_sources();

		for (auto node : ttm.nodes) {
			if (node.is_seer())
				continue;

			for (auto bit : ttm.input_bits(node))
				consumers[bit].insert(node);
		}

		for (auto us : ttm.nodes) {
			if (us.is_seer())
				continue;
			auto n = rnet_node(us);

			struct rnode_w_offset {
				rnode rn;
				int off;

				int hash() const {
					return ((uintptr_t) rn) + off;
				}
				bool operator==(rnode_w_offset other) const {
					return rn == other.rn && off == other.off;
				}
			};

			dict<pool<rnode_w_offset>,int> costmap;

			for (auto bit : ttm.output_bits(us)) {
				pool<rnode_w_offset> cs;
				for (auto leaf : db.lookup_leaves(bit)) {
					auto up = db.lookup(leaf);
					assert(up.bit == bit);
					for (auto cnode : consumers[leaf])
						cs.insert(rnode_w_offset{ rnet_node(cnode), up.offset });
				}
				costmap[cs]++;
			}

			if (us.is_timeportal()) {
				log_debug("Timeportal %s bank %s has domain %s (%d).\n",
						  log_id(us.name()), log_id(us.bank), log_id(us.timetravel_domain()),
						  n->assign);
				if (us.domain_constrained_from_above()) {
					auto domain = rnet_domain_node(us.timetravel_domain());
					rnet.add_edge(n, domain);
					log_debug("\tconstraint from above\n");
				}
				if (us.domain_constrained_from_below()) {
					auto domain = rnet_domain_node(us.timetravel_domain());
					rnet.add_edge(domain, n);
					log_debug("\tconstraint from below\n");
				}
			}

			if (us.is_backedge() && us.bank == ID::A)
				rnet.add_edge(n, rnet_node(us.partner()));

			for (auto &pair : costmap)
			switch (pair.first.size()) {
			case 0: // no consumers
				break;
			case 1: // single consumer
				{
					auto cn = *(pair.first.begin());
					rnet.add_edge(cn.rn, n, pair.second, cn.off);
				}
				break;
			default: // multiple consumers
				{
					auto aux = rnet.add_node();
					aux->label = "\\aux_" + us.name().str();
					for (auto cn : pair.first) {
						rnet.add_edge(cn.rn, n, 0, cn.off); // data edge
						rnet.add_edge(aux, cn.rn, 0, -cn.off);
					}
					rnet.add_edge(aux, n, pair.second, 0);
					assign_minimum(aux);
				}
				break;
			}
		}

		/*
		 * Domains don't have an RTLIL representation and as such their 'assign'
		 * cannot be read off from the circuit. Instead we can reliably deduce an
		 * initial assign by consulting the domain's edges, since those will be
		 * to/from circuit cells, never among two domains.
		 */
		for (auto pair : domain_nodes) {
			auto rnode = pair.second;
			assign_minimum(rnode);
			log("Warning: Making up initial placement of domain node '%s' at %d.\n",
				log_id(rnode->label), rnode->assign);
		}

		check_constraints();

		log("Have %zu nodes, cost sum %d.\n", rnet.nodes.size(), costsum(rnet));

		reset(rnet);
		auto ordered = order_sccs(rnet,
					rnet_node(timetravel_node::perimeter));
		log("Found %zu strongly-connected components.\n", ordered.size());

		check_constraints();

		log("Have %zu nodes, cost sum %d.\n", rnet.nodes.size(), costsum(rnet));

		log("Merging saturated components...\n");
		merge_saturated_components merge_saturated(rnet);
		merge_saturated.run();

		log("Have %zu nodes, cost sum %d.\n", rnet.nodes.size(), costsum(rnet));

		log("Optimizing time placement...\n");

		resolve(rnet, true);
		while (true) {
			if (!push(rnet, 1))
				break;
			resolve(rnet, false);
		}

		log("Final cost sum: %7d\n", costsum(rnet));

		check_constraints();

		auto fix = rnet.look_up_fix(rnet_node(timetravel_node::perimeter));
		int pivot = fix.upstream->assign + fix.offset;

		for (auto us : ttm.nodes) {
			auto n = rnet_node(us);
			auto fix = rnet.look_up_fix(n);
			assert(rnet.nodes.count(fix.upstream));
			us.set_timetravel_attribute(fix.upstream->assign + fix.offset - pivot);
		}
	}

	void check_constraints() {
		for (auto node : rnet.nodes)
		for (auto edge : node->edges)
			if(!edge.check())
				log_error("Edge constraint failed: (%s) %d + %d > (%s) %d\n",
						log_id(edge.endpoint->label), edge.endpoint->assign,
						edge.offset, log_id(node->label), node->assign);
	}
};

struct SeerResolvePass : Pass {
	SeerResolvePass() : Pass("seer_resolve", "plan timetravel") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_RESOLVE pass. (plan timetravel)\n");

		bool mask_undef_mux = false;
		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			auto arg = args[argidx];
			if (arg == "-mask_undef_mux")
				mask_undef_mux = true;
			else
				break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules())
			optimize_worker w(d, m, mask_undef_mux);
	}
} SeerResolvePass;

};
