// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#ifndef __GRAPHIC_GRAPH_H__
#define __GRAPHIC_GRAPH_H__

#include <vector>

#include "kernel/hashlib.h"
#include "kernel/rtlil.h"
#include "kernel/log.h"

template<typename Node, typename Edge>
class Graph {
	Graph(const Graph&) = delete;
	Graph& operator=(Graph const&) = delete;

protected:
	virtual void node_added(Node *) {}

public:
	hashlib::pool<Node*> nodes; 
	hashlib::dict<Node*, std::vector<Edge>> edges;

	static Edge reverse(const Edge &one_way)
	{
		Edge other_way = one_way.reversed();
		log_assert(one_way.from == other_way.to);
		log_assert(other_way.from == one_way.to);
		return other_way;
	}

	Graph() {};
	~Graph() {
		for (auto node : nodes)
			delete node;
	};

	template<typename... Args>
	Node *node(Args&&... args)
	{
		Node *node = new Node(args...);
		nodes.insert(node);
		node_added(node);
		return node;
	}

	template<typename... Args>
	void link(Args&&... args)
	{
		Edge edge(args...);
		edges[edge.from].push_back(edge);
		edges[edge.to].push_back(reverse(edge));
	}

	void link(Edge &&edge)
	{
		edges[edge.from].push_back(edge);
		edges[edge.to].push_back(reverse(edge));
	}
};

typedef std::vector<Yosys::IdString> Namespace;

YS_MAYBE_UNUSED static std::string format_namespace(Namespace ns)
{
	std::ostringstream stream;
	bool first = true;
	for (auto id : ns) {
		if (!first)
			stream << " ";
		else
			first = false;
		stream << id.c_str();
	}
	return stream.str();
}

template<typename Node, typename Edge>
class Subgraph {
	const Graph<Node, Edge> &carrier;
	Namespace ns;

public:
	Subgraph(Graph<Node, Edge> &carrier, Namespace ns)
		: carrier(carrier), ns(ns) {};
};

struct Immutnode;
typedef std::pair<Immutnode *, std::vector<int>> NodeIndex;

struct Namespaced {
	Namespace ns;

	bool in_namespace(Namespace outer) const
	{
		if (outer.size() > ns.size())
			return false;
		for (unsigned long i = 0; i < outer.size(); i++)
		if (outer[i] != ns[i])
			return false;
		return true;
	}
};

struct Immutnode : Namespaced {
	Yosys::IdString id;
	Yosys::SigSpec en;
	bool root = false; // TODO: remove if unused
	std::string src;

	NodeIndex index;

	Immutnode(Yosys::IdString id)
		: id(id) {};
};

template<> struct hashlib::hash_ops<Immutnode*> : ::Yosys::hashlib::hash_ptr_ops {};

struct Element {
	std::string label = "";
	bool inv = false;

	Element inversed() const {
		Element ret = *this;
		if (!empty())
			ret.inv = !inv;
		return ret;
	}

	Element() {}
	Element(std::string l, bool inv=false) : label(l), inv(inv) {}
	~Element() {}

	std::string str() const {
		return label + (inv ? "^-1" : "");
	}

	std::string canonical_str() const {
		log_assert(!inv);
		return str();
	}

	bool operator<(const Element &other) const {
		return std::tie(label, inv) < std::tie(other.label, other.inv);
	}

	bool operator==(const Element &other) const {
		return std::tie(label, inv) == std::tie(other.label, other.inv);
	}

	bool empty() const {
		return label == "";
	}

	unsigned int hash() const {
		return hashlib::hash_ops<std::string>::hash(label) + inv;
	}
};

struct Immutedge : Namespaced {
	Immutnode *from, *to;
	bool dir = 1;

	Yosys::SigSpec hot;
	int offset = 0;
	int phantom_threshold = 0;
	Yosys::IdString bseq_from;
	Yosys::IdString bseq_to;
	bool in_spantree = false;
	Element fg_element;

	Immutedge() {}

	Immutedge reversed() const
	{
		Immutedge ret(*this);
		ret.to = from;
		ret.from = to;
		ret.dir = !dir;
		ret.fg_element = fg_element.inversed();
		return ret;
	}

	bool operator<(const Immutedge &other) const {
		return std::tie(from, to, ns, dir, fg_element) \
				< std::tie(other.from, other.to, other.ns,
						   other.dir, other.fg_element);
	}

	bool operator>(const Immutedge &other) const {
		return other < *this;
	}

	bool operator!=(const Immutedge &other) const {
		return (*this < other) || (other < *this);
	}

	bool operator==(const Immutedge &other) const {
		return !(*this != other);
	}

	int dir_factor() const { return dir ? 1 : -1; }

	bool in_namespace(Namespace outer) const
	{
		return ((int) outer.size() >= phantom_threshold) && Namespaced::in_namespace(outer);
	}
};

struct InvertMap;
class Immutlinks : public Graph<Immutnode, Immutedge> {
protected:
	virtual void node_added(Immutnode *node) final
	{
		log_assert(!indexed);
		if (nodes_by_id.count(node->id))
			Yosys::log_error("Duplicate node: %s\n", node->id.c_str());
		nodes_by_id[node->id] = node;
	}

public:
	Yosys::Module *module;
	InvertMap *imap = NULL;
	hashlib::dict<Yosys::IdString, Immutnode*> nodes_by_id;
	hashlib::dict<NodeIndex, Immutnode*> nodes_by_index;
	hashlib::dict<Element, Immutedge> edges_by_fg_element;
	bool indexed = false;

	Immutlinks() {};
	~Immutlinks() {};
	bool operator==(const Immutlinks &other) const { return this == &other; }

	Immutnode *lookup_id(Yosys::IdString id)
	{
		if (!nodes_by_id.count(id))
			Yosys::log_error("No such node: %s\n", id.c_str());
		return nodes_by_id[id];
	}

	void parse(Yosys::RTLIL::Module *m);
	void save(Yosys::RTLIL::Module *m);

	static void dump_node(std::ostream &stream, const Immutnode *node);
	static void dump_edge(std::ostream &stream, const Immutedge &edge);
	void dump(std::ostream &stream);
	void parse(Yosys::RTLIL::Module *m, std::string content);

	void index();
	void check();
	void build_index(Immutnode *root);

	struct SpantreeWalk {
		Immutlinks &links;
		NodeIndex from, to;

		class iterator {
			Immutlinks &links;
			unsigned long common;
			bool down;
			NodeIndex pos, goal;
		public:
			using iterator_category = std::input_iterator_tag;
			using value_type = Immutedge;
			using difference_type = ptrdiff_t;
			using pointer = value_type*;
			using reference = value_type&;

			iterator(Immutlinks &links, NodeIndex pos, NodeIndex goal)
				: links(links), down(false), pos(pos), goal(goal)
			{
				log_assert(pos.first == goal.first);
				unsigned long max_common = std::min(pos.second.size(), goal.second.size());
				for (common = 0; common < max_common; common++)
				if (pos.second[common] != goal.second[common])
					break;
				if (pos.second.size() == common)
					down = true;
			}

			iterator& operator++()
			{
				if (down)
					goto down;

				pos.second.pop_back();
				if (pos.second.size() == common)
					down = true;
				return *this;

			down:
				pos.second.push_back(goal.second[pos.second.size()]);
				return *this;
			}

			bool operator==(const iterator &other) const
			{
				return this->pos == other.pos && this->goal == other.goal;
			}

			bool operator!=(const iterator &other) const { return !(*this == other); }

			Immutedge operator*() const
			{
				Immutedge edge;
				if (down) {
					Immutnode *upper_node = links.nodes_by_index.at(pos);
					edge = links.edges.at(upper_node).at(goal.second[pos.second.size()]);
					return edge;
				} else {
					log_assert(pos.second.size() >= 1);
					NodeIndex upper = pos;
					upper.second.pop_back();
					Immutnode *upper_node = links.nodes_by_index.at(upper);
					edge = links.edges.at(upper_node).at(pos.second.back());
					return Immutlinks::reverse(edge);
				}
			}
		};

		iterator begin() const { return iterator(links, from, to); };
		iterator end() const   { return iterator(links, to, to); };
	};

	SpantreeWalk walk_spantree(Immutnode *from, Immutnode *to)
	{
		log_assert(indexed);
		return SpantreeWalk{ *this, from->index, to->index };
	}
};

#endif /* __GRAPHIC_GRAPH_H__ */
