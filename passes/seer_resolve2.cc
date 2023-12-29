// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <vector>
#include <queue>

#include "kernel/sigtools.h"
#include "kernel/celltypes.h"

USING_YOSYS_NAMESPACE

//FIXME: not in private namespace because of hashlib specialization
//PRIVATE_NAMESPACE_BEGIN

#include "shared.h"

using hashlib::hash_ops;
using hashlib::hash_cstr_ops;
using hashlib::hash_ptr_ops;
using hashlib::hash_obj_ops;

namespace resolve2 {

#include "timetravel.h"
struct node;

/*
 * Each constraint corresponds to a variable in the simplex algorithm.
 */

struct constraint {
	std::map<node *, int> signature;
	void bump_signature(node *n, int s);

	int cost = 0;
	int offset = 0;

	int  sum();
	int  push_sum();
	bool trivial();
	bool simple()     { return signature.size() == 2; }
	int  leeway()	  { return sum() - offset; }
	bool saturated()  { return !leeway(); };
	bool check()      { return leeway() >= 0; };
	int  nunvisited();
	int  nvisited();

	std::string dump();

	// simplex variables
	bool solid = false;
	int cost_amended = 0;
};

bool constraint::trivial()
{
	for (auto &pair : signature)
	if (pair.second != 0)
		return false;
	return true;
}

void constraint::bump_signature(node *n, int s)
{
	if (!signature.count(n))
		signature[n] = 0;
	signature[n] += s;
}

typedef node* rnode;
typedef constraint* redge;

// exit the namespace to specialize hash_ops
};
template<> struct ::Yosys::hashlib::hash_ops<resolve2::rnode> : ::Yosys::hashlib::hash_ptr_ops {};
template<> struct ::Yosys::hashlib::hash_ops<resolve2::redge> : ::Yosys::hashlib::hash_ptr_ops {};
namespace resolve2 {
// we are back

struct node {
	IdString label;
	int assign;
	std::vector<redge> edges;
	bool visited = false;
	int pressure;
	int push;

	node() {}
};

int constraint::sum()
{
	int ret = 0;
	for (auto pair : signature)
		ret += pair.first->assign * pair.second;
	return ret;
}

int constraint::push_sum()
{
	int ret = 0;
	for (auto pair : signature)
		ret += pair.first->push * pair.second;
	return ret;
}

std::string constraint::dump()
{
	std::string ret = "";

	for (auto pair : signature) {
		node *n = pair.first;
		ret += stringf("%d*%s(%d) ", pair.second, log_id(n->label), n->assign);
	}

	ret += stringf("> %d", offset);
	return ret;
}

int constraint::nunvisited()
{
	int ret = 0;
	for (auto pair : signature)
	if (!pair.first->visited)
		ret++;
	return ret;
}

int constraint::nvisited()
{
	int ret = 0;
	for (auto pair : signature)
	if (pair.first->visited)
		ret++;
	return ret;
}

struct resolvnet {
	rnode pivot;

	pool<rnode> nodes;
	pool<redge> edges;
	int costlapse = 0;

	// TODO: remove copy constructor

	~resolvnet()
	{
		for (auto n : nodes)
			delete n;
		nodes.clear();
		for (auto c : edges)
			delete c;
		edges.clear();
	}

	node *add_node()
	{
		node *n = new node;
		nodes.insert(n);
		return n;
	}

	redge constrain(constraint cont)
	{
		if (cont.trivial()) {
			log_assert(cont.check());
			return NULL;
		}

		constraint *c = new constraint(cont);
		edges.insert(c);
		for (auto pair : c->signature)
			pair.first->edges.push_back(c);

		return c;
	}

	redge add_edge(node *from, node *to, int cost=0, int offset=0)
	{
		constraint edge;
		edge.offset = offset;
		edge.cost = cost;
		costlapse -= offset * cost;
		edge.bump_signature(from, +1);
		edge.bump_signature(to, -1);

		if (edge.trivial() && !edge.check())
			log_error("Bad edge: from %s to itself with offset %d.\n",
					  log_id(from->label), offset);

		return constrain(edge);
	}
};

int costsum(resolvnet &rnet) {
	int sum = 0;
	for (auto node : rnet.nodes)
	for (auto edge : node->edges) {
		auto info = edge->dump();
		if (edge->signature[node] < 0)
			continue;
		sum += edge->sum() * edge->cost;
	}
	return sum + rnet.costlapse;
}

// Find a component connected by satured edges
// (only proper edges, no multi-way constraints)
pool<rnode> component(rnode seed)
{
	pool<rnode> comp;
	pool<rnode> qu;
	comp.insert(seed);
	qu.insert(seed);

	while (!qu.empty()) {
		auto front = qu.pop();
		comp.insert(front);
		for (auto edge : front->edges)
		if (edge->saturated() && edge->simple())
		for (auto pair : edge->signature) {
			auto node = pair.first;
			if (comp.count(node))
				continue;
			edge->solid = true;
			comp.insert(node);
			qu.insert(node);
		}
	}

	return comp;
}

bool is_pm_one(int no)
{
	return no == -1 || no == 1;
}

void check_constraints(resolvnet &rnet) {
	for (auto node : rnet.nodes)
	for (auto edge : node->edges)
	if (!edge->check()) {
		std::string edge_info = edge->dump();
		log_error("Edge constraint failed: %s", edge_info.c_str());
	}
}

// Find an initial spanning tree
void span(resolvnet &rnet)
{
	pool<rnode> visited;
	std::vector<pool<rnode>> components;

	for (auto node : rnet.nodes)
	for (auto cont : node->edges)
		log_assert(cont->simple());

	for (auto node : rnet.nodes) {
		if (visited.count(node))
			continue;
		auto compo = component(node);
		for (auto n : compo)
			visited.insert(n);
		components.push_back(compo);
	}

	log_debug("Found %zd saturated components.\n", components.size());

	while (components.size() > 1) {
		// Take one of the non-pivot components
		int i;
		for (i = 0; i < (int) components.size(); i++)
		if (!components[i].count(rnet.pivot))
			break;

		int extent = std::numeric_limits<int>::max();
		redge bridge = NULL;
		for (auto node : components[i])
		for (auto cont : node->edges) {
			int stride = 0;
			for (auto pair : cont->signature)
			if (components[i].count(pair.first))
				stride += pair.second;
			if (!stride) continue;
			log_assert(is_pm_one(stride));

			int edge_extent = -cont->leeway() / stride;
			if (std::abs(edge_extent) < std::abs(extent)) {
				extent = edge_extent;
				bridge = cont;
			}
		}

		if (bridge == NULL) {
			/*
			 * No bridge? Make up one.
			 */

			auto arbitrary = *components[i].element(0);
			bridge = rnet.add_edge(arbitrary, rnet.pivot,
								   0, arbitrary->assign - rnet.pivot->assign);
			extent = 0;
		}

		bridge->solid = true;

		if (extent)
			log_debug("Moving component %d of %zu nodes by %d\n",
					  i, (size_t) components[i].size(), extent);

		for (auto node : components[i])
			node->assign += extent;

		rnode other_node = NULL;
		for (auto pair : bridge->signature)
		if (!components[i].count(pair.first))
			other_node = pair.first;
		log_assert(other_node != NULL);

		int j;
		for (j = 0; j < (int) components.size(); j++)
		if (j != i && components[j].count(other_node))
			break;

		for (auto node : components[i])
			components[j].insert(node);
		components.erase(components.begin() + i);
	}

	log_assert(components.size() == 1);

	int nsolid = 0;
	for (auto edge : rnet.edges)
	if (edge->solid)
		nsolid++;

	log_debug("Have %zd edges, of those %d solid.\n", rnet.edges.size(), nsolid);
}

void reset_visited(resolvnet &rnet)
{
	for (auto node : rnet.nodes)
		node->visited = false;
}

bool walk(resolvnet &rnet)
{
	std::vector<rnode> order;
	reset_visited(rnet);
	rnet.pivot->visited = true;
	order.push_back(rnet.pivot);

	/*
	 * Walk the spanning tree and order the nodes in such a way
	 * that the nodes that come up later in the spanning tree
	 * are ordered behind those who come earlier.
	 */
	for (int i = 0; i < (int) order.size(); i++) {
		rnode front = order[i];

		/*
		 * Check that all but a single node in the constraint
		 * have not been visited, and if so, queue up that node
		 * for visitation.
		 */
		for (auto cont : front->edges) {
			if (!cont->solid || cont->nunvisited() != 1)
				continue;

			for (auto pair : cont->signature)
			if (!pair.first->visited) {
				pair.first->visited = true;
				order.push_back(pair.first);		
			}
		}
	}

	// TODO
	/*
	 * There are scenarios in which the assert below we fail, we
	 * need to address those eventually.
	 */
	log_assert(order.size() == rnet.nodes.size());

	/*
	 * Reset the node pressure to the amount that's due to the
	 * immediate non-solid edges the node is participating in.
	 */
	for (auto node : rnet.nodes) {
		int pressure = 0;
		for (auto cont : node->edges)
		if (!cont->solid)
			pressure += cont->signature[node] * cont->cost;
		node->pressure = pressure;
	}

	/*
	 * Now walk the nodes in the reverse order, spreading the
	 * pressure down solid edges, and finding the amended cost
	 * on the solid edges.
	 */
	reset_visited(rnet);

	for (auto node = order.rbegin(); node != order.rend(); node++) {
		for (auto cont : (*node)->edges) {
			if (!cont->solid || cont->nvisited())
				continue;

			int leeway_p = cont->signature[*node] * (*node)->pressure;
			cont->cost_amended = cont->cost + leeway_p;

			for (auto pair : cont->signature) {
				if (pair.first == *node)
					continue;
				log_assert(is_pm_one(pair.second));
				pair.first->pressure -= leeway_p / pair.second;
			}
		}

		(*node)->visited = true;
	}

	/*
	 * Finally, find the solid edge to snip at.
	 */
	redge snip = NULL;
	for (auto cont : rnet.edges)
	if (cont->solid && cont->cost_amended < 0 \
			&& (!snip || cont->cost_amended < snip->cost_amended))
		snip = cont;

	if (!snip) {
		log("Done.\n");
		return false;
	}

	reset_visited(rnet);
	for (auto node : order) {
		node->push = 0;

		/*
		 * Find the solid edge by which we are holding onto
		 * the bulk of the network.
		 */
		for (auto cont : node->edges)
		if (cont->solid && cont->nunvisited() == 1) {
			/*
			 * Determine our push.
			 */
			if (cont == snip) {
				node->push = cont->signature[node];
			} else {
				log_assert(is_pm_one(cont->signature[node]));
				node->push = -cont->push_sum() / cont->signature[node];
			}
		}

		node->visited = true;
	}

	snip->solid = false;

	int max_extent = std::numeric_limits<int>::max();
	redge new_solid = NULL;
	for (auto cont : rnet.edges)
	if (!cont->solid && cont->push_sum() < 0) {
		log_assert(cont->leeway() % cont->push_sum() == 0); // TODO
		int extent = -cont->leeway() / cont->push_sum();

		if (extent < max_extent) {
			new_solid = cont;
			max_extent = extent;
		}
	}

	if (max_extent)
		log("Pressure: %3d Extent: %2d Cost sum: %4d\n",
			-snip->cost_amended, max_extent, costsum(rnet));

	for (auto node : order)
		node->assign += node->push * max_extent;
	new_solid->solid = true;

	return true;
}

struct optimize_worker {
	resolvnet rnet;
	CellTypes ct;
	SigMap sigmap;

	timetravel_module ttm;

	struct ttbit {
		int port;
		int bitidx;

		int hash() const
		{
			return 1024 * port + bitidx;
		}

		bool operator==(ttbit other) const
		{
			return port == other.port && bitidx == other.bitidx;
		}
	};

	dict<std::pair<timetravel_node, ttbit>,rnode> from_tt_nodes;

	dict<IdString,rnode> domain_nodes;

	rnode rnet_node(timetravel_node tt_node)
	{
		if (!from_tt_nodes.count(std::make_pair(tt_node, ttbit{}))) {
			auto node = rnet.add_node();
			from_tt_nodes[std::make_pair(tt_node, ttbit{})] = node;
			node->label = tt_node.name();
			if (tt_node.cell)
				node->assign = tt_node.get_timetravel_attribute();
			else
				node->assign = 0;
			return node;
		}
		return from_tt_nodes[std::make_pair(tt_node, ttbit{})];
	}

	rnode rnet_domain_node(IdString label)
	{
		if (!domain_nodes.count(label)) {
			auto node = rnet.add_node();
			node->label = label;
			// TODO: fix-up in case of unconstrained domains,
			//		 as we cannot save domain's assign like we
			//		 can store other assigns
			node->assign = std::numeric_limits<int>::min();
			domain_nodes[label] = node;
			return node;
		}
		return domain_nodes[label];
	}

	void assign_minimum(rnode node)
	{
		int max = std::numeric_limits<int>::min();
		for (auto& edge : node->edges) {
			log_assert(edge->signature[node] != 0);
			if (edge->signature[node] < 0)
				continue;
			max = std::max(max, node->assign - edge->leeway() \
								/ edge->signature[node]);
		}
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
				rnode rnode;
				int off;

				int hash() const {
					return ((uintptr_t) rnode) + off;
				}
				bool operator==(rnode_w_offset other) const {
					return rnode == other.rnode && off == other.off;
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
					rnet.add_edge(cn.rnode, n, pair.second, cn.off);
				}
				break;
			default: // multiple consumers
				{
					auto aux = rnet.add_node();
					aux->label = "\\aux_" + us.name().str();
					for (auto cn : pair.first) {
						rnet.add_edge(cn.rnode, n, 0, cn.off); // data edge
						rnet.add_edge(aux, cn.rnode, 0, -cn.off);
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

		check_constraints(rnet);

		rnet.pivot = rnet_node(timetravel_node::perimeter);
		log("Have %zu nodes, cost sum %d.\n", rnet.nodes.size(), costsum(rnet));
		span(rnet);
		while (walk(rnet));
		log("Have %zu nodes, cost sum %d.\n", rnet.nodes.size(), costsum(rnet));

		check_constraints(rnet);

		int pivot = rnet.pivot->assign;

		for (auto us : ttm.nodes) {
			auto n = rnet_node(us);
			assert(rnet.nodes.count(n));
			us.set_timetravel_attribute(n->assign - pivot);
		}
	}
};

struct SeerResolve2Pass : Pass {
	SeerResolve2Pass() : Pass("seer_resolve2", "plan timetravel") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing SEER_RESOLVE2 pass. (plan timetravel)\n");

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
} SeerResolve2Pass;

};
