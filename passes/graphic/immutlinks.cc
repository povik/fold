// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <iomanip>
#include <iostream>

#include "kernel/rtlil.h"
#include "kernel/sigtools.h"

#include "graph.h"
#include "quotient.h"

USING_YOSYS_NAMESPACE;

void Immutlinks::parse(Module *m) {
	if (m->design->scratchpad_get_string("immutlinks") == "")
		return;
	parse(m, m->design->scratchpad_get_string("immutlinks"));
}

void Immutlinks::save(Module *m) {
	std::stringstream ss;
	dump(ss);
	m->design->scratchpad_set_string("immutlinks", ss.str());
}

bool consume(std::istream &stream, const char *str)
{
	int len = strlen(str);
	int pos = 0;
	while (pos < len) {
		int ch;
		if ((ch = stream.get()) == str[pos]) {
			pos++;
		} else {
			pos--;
			if (!stream.eof())
				stream.putback(ch);
			goto back;
		}
	}
	return true;

back:
	for (; pos >= 0; pos--)
	if (!stream.eof())
		stream.putback(str[pos]);
	return false;
}

int parse_int(std::istream &stream)
{
	int ch;
	int num = 0;
	bool once = false;
	bool negative = false;
	if (consume(stream, "-")) negative = true;
	while ((ch = stream.get()) >= '0' && ch <= '9') {
		once = true;
		num = 10 * num + (ch - '0');
	}
	log_assert(once);
	stream.putback(ch);
	return negative ? -num : num;
}

std::string parse_string(std::istream &stream)
{
	log_assert(stream.get() == '\'');
	std::string ret;

	int ch;
	while ((ch = stream.get()) != '\'') {
		log_assert(!stream.eof());
		if (ch == '\\') {
			ret.push_back(stream.get());
			continue;
		}
		ret.push_back(ch);
	}

	return ret;
}

IdString parse_id(std::istream &stream)
{
	std::string ret;
	int ch;
	while ((ch = stream.get()) > ' ') {
		log_assert(!stream.eof());
		ret.push_back(ch);
	}
	stream.putback(' ');
	return IdString(ret);
}

void separator(std::istream &stream, bool required=true)
{
	bool once = false;
	int ch;
	while (true) {
		switch (ch = stream.get()) {
		case '\n': case '\r': case ' ': case '\t':
			once = true;
			continue;
		default:
			log_assert(once || !required);
			stream.putback(ch);
			return;
		}
	}
}

void dump_bit(std::ostream &stream, SigBit bit)
{
	log_assert(bit.wire);
	stream << bit.wire->name.c_str();
	if (bit.wire->width != 1)
		stream << " bitpos " << bit.offset;
	stream << " end\n";
}

void Immutlinks::dump_node(std::ostream &stream, const Immutnode *node)
{
	stream << "  node " << node->id.c_str() << " \n";
	if (!node->ns.empty()) {
		stream << "    ns ";
		for (auto ns : node->ns)
			stream << ns.c_str() << " ";
		stream << "end\n";
	}
	stream << "    en ";
	dump_bit(stream, node->en);
	if (node->root) stream << "  root\n";
	if (node->index.first) {
		stream << "    # index " << node->index.first->id.c_str();
		for (auto i : node->index.second)
			stream << " " << i;
		stream << "\n";
	}
	if (!node->src.empty()) {
		stream << "    src '";
		stream << node->src;
		stream << "'\n";
	}
	stream << "  end\n";
}

void Immutlinks::dump_edge(std::ostream &stream, const Immutedge &edge)
{
	if (!edge.dir) stream << "  # XXX reversed edge\n";
	stream << "  edge " << edge.from->id.c_str() << " " << edge.to->id.c_str() << "\n";
	if (!edge.ns.empty()) {
		stream << "    ns ";
		for (auto ns : edge.ns)
			stream << ns.c_str() << " ";
		stream << "end\n";
	}
	if (edge.hot != SigBit(State::S1)) {
		stream << "    hot ";
		dump_bit(stream, edge.hot.as_bit());
	}
	if (edge.offset != 0)		stream << "    offset " << edge.offset << "\n";
	if (edge.phantom_threshold)	stream << "    phantom_threshold " << edge.phantom_threshold << "\n";
	if (!edge.bseq_from.empty() || !edge.bseq_to.empty()) {
		log_assert(!edge.bseq_from.empty() && !edge.bseq_to.empty());
		stream << "    bseq " << edge.bseq_from.c_str() << " " \
			   << edge.bseq_to.c_str() << "\n";
	}
	if (edge.in_spantree) 			stream << "    in_spantree\n";
	if (!edge.fg_element.empty())	stream << "    fg_element '" << edge.fg_element.canonical_str() << "'\n";
	stream << "  end\n";
}

void Immutlinks::dump(std::ostream &stream) {
	stream << "immutlinks\n";
	for (auto node : nodes) {
		dump_node(stream, node);
	}
	for (auto pair : edges)
	for (auto edge : pair.second) {
		if (!edge.dir)
			continue;
		dump_edge(stream, edge);
	}
}

SigBit parse_bit(Module *m, std::istream &stream) {
	Wire *w = m->wire(parse_id(stream));
	log_assert(w);
	separator(stream);
	int idx = 0;
	if (consume(stream, "bitpos")) {
		separator(stream);
		idx = parse_int(stream);
		separator(stream);
	}
	log_assert(consume(stream, "end"));
	return SigBit(w, idx);
}

void Immutlinks::parse(Module *m, std::string content) {
	std::stringstream stream(content);
	log_assert(nodes.empty());
	log_assert(edges.empty());

	log_assert(consume(stream, "immutlinks"));
	while (true) {
		separator(stream);
		if (consume(stream, "node")) {
			separator(stream);
			Immutnode *n = node(parse_id(stream));
			while (true) {
				separator(stream);
				if (consume(stream, "ns")) {
					separator(stream);
					while (!consume(stream, "end")) {
						log_assert(!stream.eof());
						n->ns.push_back(parse_id(stream));
						separator(stream);
					}
				} else if (consume(stream, "end")) {
					break;
				} else if (consume(stream, "en")) {
					separator(stream);
					n->en = parse_bit(m, stream);
				} else if (consume(stream, "root")) {
					n->root = true;
				} else if (consume(stream, "src")) {
					separator(stream);
					n->src = parse_string(stream);
				} else if (consume(stream, "#")) {
					while (!stream.eof() && stream.get() != '\n');
				} else {
					log_assert(false && "bad node attribute");
				}
			}
		} else if (consume(stream, "edge")) {
			Immutedge edge;
			separator(stream);
			edge.from = lookup_id(parse_id(stream));
			separator(stream);
			edge.to = lookup_id(parse_id(stream));
			edge.hot = SigBit(State::S1);
			while (true) {
				separator(stream);
				if (consume(stream, "ns")) {
					separator(stream);
					while (!consume(stream, "end")) {
						log_assert(!stream.eof());
						edge.ns.push_back(parse_id(stream));
						separator(stream);
					}
				} else if (consume(stream, "hot")) {
					separator(stream);
					edge.hot = parse_bit(m, stream);
				} else if (consume(stream, "offset")) {
					separator(stream);
					edge.offset = parse_int(stream);
				} else if (consume(stream, "phantom_threshold")) {
					separator(stream);
					edge.phantom_threshold = parse_int(stream);
				} else if (consume(stream, "bseq")) {
					separator(stream);
					edge.bseq_from = parse_id(stream);
					separator(stream);
					edge.bseq_to = parse_id(stream);
				} else if (consume(stream, "in_spantree")) {
					edge.in_spantree = true;
				} else if (consume(stream, "fg_element")) {
					separator(stream);
					edge.fg_element = Element(parse_string(stream));
				} else if (consume(stream, "#")) {
					while (!stream.eof() && stream.get() != '\n');
				} else if (consume(stream, "end")) {
					break;
				} else {
					log_assert(false && "bad edge attribute");
				}
			}
			link(edge);
		} else if (consume(stream, "#")) {
			while (!stream.eof() && stream.get() != '\n');
		} else {
			break;
		}
	}
	separator(stream, false);
	log_assert(stream.get() == -1);
}

void Immutlinks::index() {
	for (auto node : nodes) {
		for (const auto &edge : edges[node])
		if (!edge.in_spantree)
			edges_by_fg_element[edge.fg_element] = edge;

		if (node->index.first)
			continue; // indexed
		log_debug("Indexing from root %s\n", log_id(node->id));	
		build_index(node);
		log_debug("Index at %zd\n", nodes_by_index.size());	
	}
	indexed = true;
}

void Immutlinks::check() {
	for (auto node : nodes)
	for (const auto &edge : edges[node]) {
		if (!edge.bseq_from.empty() || !edge.bseq_to.empty()) {
			log_assert(nodes_by_id.count(edge.bseq_from));
			log_assert(nodes_by_id.count(edge.bseq_to));
		}
	}
}

void Immutlinks::build_index(Immutnode *root)
{
	for (auto node : nodes) {
		std::vector<Immutedge> &node_edges = edges[node];
		std::sort(node_edges.begin(), node_edges.end(), [](Immutedge &a, Immutedge &b) {
			if (a.dir && !b.dir) return true; // first sort from dir set to unset
			if (a.in_spantree && !b.in_spantree) return true; // then from in_spantree set to unset
			return false;
		});
	}

	std::vector<int> curr_index = { 0 };
	std::vector<Immutnode *> stack;
	stack.push_back(root);
	root->index = NodeIndex(root, {});
	nodes_by_index[NodeIndex(root, {})] = root;
	while (true) {
		auto &local_edges = edges[stack.back()];
		int edge_select = curr_index.back();

		if (edge_select >= (int) local_edges.size()) {
			curr_index.pop_back();
			stack.pop_back();
			if (curr_index.empty())
				break;
			curr_index[curr_index.size() - 1]++;
			continue;
		}

		auto &edge = local_edges[edge_select];
		if (!edge.in_spantree /* || !edge.dir */ || \
				(stack.size() >= 2 && edge.to == stack[stack.size() - 2])) {
			curr_index[curr_index.size() - 1]++;
			continue;
		}

		Immutnode *next = local_edges[edge_select].to;
		next->index = NodeIndex(root, curr_index);
		nodes_by_index[NodeIndex(root, curr_index)] = next;
		stack.push_back(next);
		curr_index.push_back(0);
	}
}

static SigSpec transform(Immutlinks &links, const Immutedge &edge, SigSpec data, bool zeroed=false);

struct ModuleBuilder {
	Module *m;
	ModuleBuilder(Module *m) : m(m) {};

	SigSpec LogicAnd(IdString id, SigSpec a, SigSpec b, bool signed_=false, std::string src="")
	{
		if (a == SigSpec(State::S1))
			return b;
		if (b == SigSpec(State::S1))
			return a;
		if (a == SigSpec(State::S0) || b == SigSpec(State::S0))
			return State::S0;
		return m->LogicAnd(id, a, b, signed_, src);
	}

	SigSpec ReduceBool(IdString id, SigSpec a, bool signed_=false, std::string src="")
	{
		SigSpec a_filtered;
		for (auto bit : a) {
			if (bit == State::S0)
				continue;
			if (bit == State::S1)
				return State::S1;
			a_filtered.append(bit);	
		}
		if (a_filtered.empty())
			return State::S0;
		if (a_filtered.size() == 1)
			return a_filtered;
		return m->ReduceBool(id, a_filtered, signed_, src);
	}

	SigSpec Pmux(IdString id, SigSpec a, SigSpec b, SigSpec s, std::string src="")
	{
		log_assert(a.size() * s.size() == b.size());
		SigSpec b_filtered;
		SigSpec s_filtered;
		int width = a.size();
		for (int i = 0; i < s.size(); i++) {
			if (s[i] == State::S0)
				continue;
			b_filtered.append(b.extract(width * i, width));
			s_filtered.append(s[i]);
		}
		if (s_filtered.empty())
			return a;
		return m->Pmux(id, a, b_filtered, s_filtered, src);
	}

	SigSpec Mux(IdString id, SigSpec a, SigSpec b, SigSpec s, std::string src="")
	{
		log_assert(a.size() == b.size());
		log_assert(s.size() == 1);
		if (a == b)
			return a;
		if (s == SigSpec(State::S0))
			return a;
		if (s == SigSpec(State::S1))
			return b;
		return m->Mux(id, a, b, s, src);
	}

	Cell *addReduceBool(IdString id, SigSpec a, SigSpec y, bool signed_=false, std::string src="")
	{
		SigSpec a_filtered;
		for (auto bit : a) {
			if (bit == State::S0)
				continue;
			if (bit == State::S1) {
				m->connect(y, Const(1, y.size()));
				return NULL;
			}
			a_filtered.append(bit);	
		}
		if (a_filtered.empty()) {
			m->connect(y, Const(0, y.size()));
			return NULL;
		}
		if (a_filtered.size() == 1 && y.size() == 1) {
			m->connect(y, a_filtered);
			return NULL;
		}
		return m->addReduceBool(id, a, y, signed_, src);	
	}

	Cell *addPmux(IdString id, SigSpec a, SigSpec b, SigSpec s, SigSpec y, std::string src="")
	{
		log_assert(a.size() * s.size() == b.size());
		log_assert(a.size() == y.size());
		SigSpec b_filtered;
		SigSpec s_filtered;
		int width = a.size();
		for (int i = 0; i < s.size(); i++) {
			if (s[i] == State::S0)
				continue;
			b_filtered.append(b.extract(width * i, width));
			s_filtered.append(s[i]);
		}
		if (s_filtered.empty()) {
			m->connect(y, a);
			return NULL;
		}
		return m->addPmux(id, a, b_filtered, s_filtered, y, src);
	}

	void connect(const SigSpec &lhs, const SigSpec &rhs)
	{
		m->connect(lhs, rhs);
	}
};

struct InvertMap {
	pool<std::pair<SigBit, SigBit>> hits;
	SigMap &sigmap;

	InvertMap(Module *m, SigMap &sigmap)
		: sigmap(sigmap)
	{
		for (auto cell : m->cells()) {
			//
			if (cell->type == ID($not) || (cell->type == ID($logic_not) \
					&& cell->getParam(ID::A_WIDTH).as_int() == 1
					&& cell->getParam(ID::Y_WIDTH).as_int() == 1)) {
				SigSpec a = cell->getPort(ID::A), y = cell->getPort(ID::Y);
				for (int i = 0; i < std::min(a.size(), y.size()); i++)
					register_(sigmap(a[i]), sigmap(y[i]));
			}
		}
	}

	bool operator()(SigBit a, SigBit b)
	{
		a = sigmap(a); b = sigmap(b);
		if (a < b)
			std::swap(a, b);
		return hits.count(std::make_pair(a, b));
	}

	void register_(SigBit a, SigBit b)
	{
		if (a < b)
			std::swap(a, b);
		hits.insert(std::make_pair(a, b));
	}
};

struct ImportNode {
	Immutlinks &links;
	Immutnode *image;
	std::string varname;
	std::string src;
	int width;

	ImportNode(Immutlinks &links, Immutnode *image, int width, std::string varname="unk")
		: links(links), image(image), varname(varname), width(width), injects(),
			local_links(), import_data(), import_valid()
	{
		log_assert(links.module != NULL);
		src = "forwarding variable '" + varname + "'";
		if (!image->src.empty())
			src += " at " + image->src;
	}

	bool built = false;
	std::vector<std::pair<SigBit, SigSpec>> injects;
	std::map<Immutedge,ImportNode *> local_links;
	std::map<Immutedge,SigSpec> import_data;
	std::map<Immutedge,SigBit> import_valid;

	void inject(SigBit en, SigSpec data)
	{
		log_assert(!built);
		log_assert(data.size() == width);
		injects.push_back(std::make_pair(en, data));
	}

	void inject(Cell *cell)
	{
		inject(cell->getPort(ID::EN), cell->getPort(ID::D));
	}

	static void link(ImportNode *one, ImportNode *other, const Immutedge &edge)
	{
		log_assert(one->image == edge.from && other->image == edge.to);
		log_assert(one->width == other->width);
		log_assert(one->links == other->links);
		log_assert(!one->built && !other->built);
		log_assert(one != other);

		if (one->local_links.count(edge)) {
			log_assert(one->local_links.at(edge) == other);
			log_assert(other->local_links.at(edge.reversed()) == one);
			return;
		}

		int width = one->width;
		Module *m = one->links.module;

		for (const Immutedge &l : {edge, edge.reversed()}) {
			Wire *en_wire;
			one->import_valid[l] = other->import_valid[l] = en_wire = m->addWire(NEW_ID, 1);
			en_wire->attributes[ID(seer.background_value)] = Const(0, 1);
			one->import_data[l] = other->import_data[l] = m->addWire(NEW_ID, width);
		}

		one->local_links[edge] = other;
		other->local_links[edge.reversed()] = one;
	}

	void check() {
		for (auto pair : local_links) {
			log_assert(pair.first.from == image);
			log_assert(pair.first.to == pair.second->image);
		}
	}

	void build(InvertMap *imap=NULL) {
		check();
		log_assert(!built);
		built = true;

		ModuleBuilder mb(links.module);
		for (auto const& [outward, remote] : local_links) {
			// TODO: Move this conditional inside
			if (outward.dir) {
				SigSpec B, S;
				// For downstream import, first collect all incoming
				// imports, both downstream *and* upstream
				for (auto const& [outward2, remote2] : local_links) {
					if (outward2 == outward)
						continue;
					auto inward = outward2.reversed();

					// Targeted optimization
					if (outward2.dir && imap && (*imap)(outward.hot, outward2.hot))
						continue;

					if (inward.dir)
						S.append(transform(links, inward, 
							mb.LogicAnd(NEW_ID, import_valid.at(inward), inward.hot),
						true));
					else
						S.append(mb.LogicAnd(NEW_ID,
							transform(links, inward, import_valid.at(inward), true),
							inward.hot
						));
					B.append(transform(links, inward, import_data.at(inward)));
				}
				// Now do the injects
				bool assigned = false;
				for (auto pair : injects) {
					// TODO: add sigmap
					if (pair.first == SigBit(State::S1)) {
						mb.connect(import_valid.at(outward), image->en);
						mb.connect(import_data.at(outward), pair.second);
						assigned = true;
						break;
					}

					S.append(mb.LogicAnd(NEW_ID, pair.first, image->en));
					B.append(pair.second);
				}
				if (assigned) continue;
				mb.addPmux(
					NEW_ID, SigSpec(State::Sx, width),
					B, S, import_data.at(outward), src
				);
				mb.addReduceBool(
					NEW_ID, S, import_valid.at(outward),
					false, src
				);
			} else { /* !outward.dir */
				SigSpec B, S;
				// For upstream import, only collect incoming imports
				// which are also upstream
				for (auto const& [outward2, remote2] : local_links) {
					if (outward2 == outward || !outward2.dir)
						continue;
					auto inward = outward2.reversed();
					S.append(mb.LogicAnd(NEW_ID,
						transform(links, inward, import_valid.at(inward), true),
						inward.hot, false, src
					));
					B.append(transform(links, inward, import_data.at(inward)));
				}
				// Now do the injects
				bool assigned = false;
				for (auto pair : injects) {
					// TODO: add sigmap
					if (pair.first == SigBit(State::S1)) {
						mb.connect(import_valid.at(outward), image->en);
						mb.connect(import_data.at(outward), pair.second);
						assigned = true;
						break;
					}

					S.append(mb.LogicAnd(NEW_ID, pair.first, image->en));
					B.append(pair.second);
				}
				if (assigned) continue;
				mb.addPmux(
					NEW_ID, SigSpec(State::Sx, width),
					B, S, import_data.at(outward), src
				);
				mb.addReduceBool(
					NEW_ID, S, import_valid.at(outward),
					false, src
				);
			}
		}
	}

	std::pair<SigBit, SigSpec> evaluate(bool zeroed=false)
	{
		ModuleBuilder mb(links.module);
		SigSpec B, S;
		for (auto const& [outward, remote] : local_links) {
			auto inward = outward.reversed();
			if (inward.dir)
				S.append(transform(links, inward, mb.LogicAnd(NEW_ID,
					import_valid[inward],
					inward.hot),
				true));
			else
				S.append(mb.LogicAnd(NEW_ID,
					transform(links, inward, import_valid[inward], true),
					inward.hot, false, src
				));
			B.append(transform(links, inward, import_data.at(inward)));
		}
		for (auto pair : injects) {
			S.append(mb.LogicAnd(NEW_ID, pair.first, image->en, false, src));
			B.append(pair.second);
		}
		return std::make_pair(
			mb.ReduceBool(NEW_ID, S, false, src),
			mb.Mux(
				NEW_ID, SigSpec(zeroed ? State::S0 : State::Sx, width),
				mb.Pmux(
					NEW_ID, SigSpec(zeroed ? State::S0 : State::Sx, width),
					B, S, src
				), image->en, src
			)
		);
	}

	SigSpec evaluate_data(bool zeroed=false)
	{
		SigSpec data;
		std::tie(std::ignore, data) = evaluate(zeroed);
		return data;
	}
};

static SigSpec transform(Immutlinks &links, const Immutedge &edge, SigSpec data, bool zeroed)
{
	Module *m = links.module;

	if (edge.offset != 0) {
		int offset = edge.offset * edge.dir_factor();
		SigSpec ret = m->addWire(NEW_ID, data.size());
		Cell *seer = m->addCell(NEW_ID, ID(SEER));
		seer->setParam(ID::WIDTH, data.size());
		seer->setParam(ID::OFFSET, offset);
		seer->setPort(ID::A, data);
		seer->setPort(ID::Y, ret);
		return ret;
	} else if (!edge.bseq_from.empty()) {
		Immutnode *from = links.lookup_id(edge.bseq_from);
		Immutnode *to = links.lookup_id(edge.bseq_to);
		if (!edge.dir) std::swap(from, to);
		log_assert(from->ns == to->ns);
		Namespace ns = from->ns;

		::hashlib::dict<Immutnode*, ImportNode*> import_nodes;
		for (auto node : links.nodes)
		if (node->in_namespace(ns))
			import_nodes[node] = new ImportNode(links, node, data.size());

		for (auto node : links.nodes)
		if (node->in_namespace(ns))
		for (auto const &edge : links.edges[node])
		if (edge.dir && edge.in_namespace(ns))
			ImportNode::link(import_nodes[edge.from], import_nodes[edge.to], edge);

		import_nodes[from]->inject(SigBit(State::S1), data);

		for (auto pair : import_nodes)
			pair.second->build(links.imap); // TODO: move `imap` out of Immutlinks

		SigSpec eval = import_nodes[to]->evaluate_data(zeroed);

		for (auto pair : import_nodes)
			delete pair.second;

		return eval;
	} else {
		return data;
	}
}

Namespace cell_namespace(Cell *cell)
{
	Namespace ret;
	std::string param = cell->getParam(ID(NAMESPACE)).decode_string();
	std::istringstream stream(param);
	std::string piece;
	while (getline(stream, piece, ' '))
		ret.push_back(IdString(piece));
	return ret;
}

Immutnode *cell_immutnode(Cell *cell, Immutlinks &links, IdString paramname=ID(AT_NODE))
{
	return links.lookup_id(IdString(cell->getParam(paramname).decode_string()));
}

struct ImmutvarsPass : Pass {
	ImmutvarsPass() : Pass("immutvars", "implement immutable variables") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMMUTVARS pass.\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_whole_modules_warn()) {
			log_debug("Visiting module %s.\n", log_id(m->name));
			Immutlinks links;
			links.parse(m);
			links.module = m;
			SigMap sigmap(m);
			InvertMap imap(m, sigmap);
			links.imap = &imap;
			dict<std::pair<Namespace, std::string>, std::vector<Cell*>> cells;

			for (auto cell : m->cells()) {
				if (!cell->type.in(ID(VAR_SET), ID(VAR_GET)))
					continue;

				auto id = std::make_pair(cell_namespace(cell), cell->getParam(ID(NAME)).decode_string());
				cells[id].push_back(cell);
			}

			for (auto pair : cells) {
				Namespace ns;
				std::string varname;
				std::tie(ns, varname) = pair.first;

				std::string ns_str = format_namespace(ns);
				log_debug("Implementing variable '%s' in namespace %s\n", varname.c_str(), ns_str.c_str());

				std::vector<Cell *> &var_cells = pair.second;
				log_assert(!var_cells.empty());
				int width = var_cells[0]->getParam(ID::WIDTH).as_int();

				::hashlib::dict<Immutnode*, ImportNode*> import_nodes;
				for (auto node : links.nodes)
				if (node->in_namespace(ns))
					import_nodes[node] = new ImportNode(links, node, width, varname);

				for (auto cell : var_cells)
				if (cell->type == ID(VAR_SET)) {
					Immutnode *node = cell_immutnode(cell, links);
					log_debug("\tset cell %s at %s\n", log_id(cell->name), log_id(node->id));
					log_assert(node->in_namespace(ns));
					import_nodes[node]->inject(cell);
				}

				for (auto node : links.nodes)
				if (node->in_namespace(ns))
				for (auto const &edge : links.edges[node])
				if (edge.dir && edge.in_namespace(ns)) {
					log_debug("\tedge %s %s (offset %d)\n", edge.from->id.c_str(), edge.to->id.c_str(), edge.offset);
					ImportNode::link(import_nodes[edge.from], import_nodes[edge.to], edge);
				}

				for (auto pair : import_nodes)
					pair.second->build(&imap);

				for (auto cell : var_cells)
				if (cell->type == ID(VAR_GET)) {
					Immutnode *node = cell_immutnode(cell, links);
					log_debug("\tget cell %s at %s\n", log_id(cell->name), log_id(node->id));
					log_assert(node->in_namespace(ns));
					SigSpec data;
					std::tie(std::ignore, data) = import_nodes[node]->evaluate();
					m->connect(cell->getPort(ID::Q), data);
					m->remove(cell);
				}

				for (auto pair : import_nodes)
					delete pair.second;
			}
		}
	}
} ImmutvarsPass;

struct ImmutvarsClean : Pass {
	ImmutvarsClean() : Pass("immutvars_clean", "clean up unused immut var sets") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMMUTVARS_CLEAN pass.\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		int nunused = 0;
		for (auto m : d->selected_whole_modules_warn()) {
			log_debug("Visiting module %s.\n", log_id(m->name));

			dict<std::pair<Namespace, std::string>, std::vector<Cell*>> cells;

			for (auto cell : m->cells()) {
				if (!cell->type.in(ID(VAR_SET), ID(VAR_GET)))
					continue;

				auto id = std::make_pair(cell_namespace(cell), cell->getParam(ID(NAME)).decode_string());
				cells[id].push_back(cell);
			}

			for (auto pair : cells) {
				std::vector<Cell *> &var_cells = pair.second;
				
				bool used = false;
				for (auto cell : var_cells)
				if (cell->type == ID(VAR_GET))
					used = true;

				if (!used)
				for (auto cell : var_cells)
				if (cell->type == ID(VAR_SET)) {
					nunused++;
					log_debug("Removing unused %s cell %s\n", log_id(cell->type), log_id(cell->name));
					m->remove(cell);
				}
			}
		}

		log("Removed %d unused VAR_SET cells.\n", nunused);
	}
} ImmutvarsClean;

std::vector<Immutedge> parse_fg_element(Immutlinks &links, std::string str)
{
	log_debug("parsing %s\n", str.c_str());

	std::vector<Immutedge> ret;
	std::stringstream stream(str);
	while (true) {
		int ch = stream.get();
		if (stream.eof()) break;

		std::string elem_str = {(char) ch};
		if (ch == '$') {
			while (isdigit(ch = stream.get()))
				elem_str.push_back(ch);
			if (!stream.eof())
				stream.putback(ch);
		}
		Element elem(elem_str);

		if (consume(stream, "^-1"))
			elem.inv = true;

		if (!links.edges_by_fg_element.count(elem)) {
			std::string elem_str = elem.str();
			log_error("No edge found for element %s\n", elem_str.c_str());
		}

		ret.push_back(links.edges_by_fg_element.at(elem));
	}
	return ret;
}

class ImportTree {
	Immutlinks &links;
	int width;
	std::string varname;

	::hashlib::dict<Immutnode *, ImportNode *> import_nodes;
	::hashlib::dict<Immutnode *, Immutnode *> implemented_roots;

	void populate(Immutnode *node)
	{
		if (!import_nodes.count(node))
			import_nodes[node] = new ImportNode(links, node, width, varname);
	}

public:
	ImportTree(Immutlinks &links, int width, std::string varname)
		: links(links), width(width), varname(varname) {};
	~ImportTree() {
		for (auto node : import_nodes)
			delete node.second;
	};
	void build(InvertMap *imap) {
		for (auto node : import_nodes)
			node.second->build(imap);
	}

	static Immutnode *common_root(Immutlinks &links, const Immutnode *a, const Immutnode *b)
	{
		log_assert(links.indexed);
		log_assert(a->index.first == b->index.first);

		const NodeIndex &ai = a->index, &bi = b->index;
		unsigned long common = 0;
		for (; common < std::min(ai.second.size(), bi.second.size()); common++)
		if (ai.second[common] != bi.second[common])
			break;

		std::vector<int> index_nos(ai.second.begin(), ai.second.begin() + common);
		return links.nodes_by_index.at(NodeIndex(ai.first, index_nos));
	}

	ImportNode *operator[](Immutnode *node) {
		if (!import_nodes.count(node)) {
			// Get the root of the tree to which `node` belongs
			Immutnode *tree_root = node->index.first;
			// Figure out the new root of the implemented subtree
			if (!implemented_roots.count(tree_root)) {
				populate(node);
				implemented_roots[tree_root] = node;
			}
			Immutnode *old_root = implemented_roots.at(tree_root);
			Immutnode *new_root = common_root(links, old_root, node);
			implemented_roots[tree_root] = new_root;

			log_debug("Linking up old (%s) and new (%s) subtree roots\n",
					  old_root->id.c_str(), new_root->id.c_str());

			// Link up new and old subtree roots
			for (const auto &edge : links.walk_spantree(old_root, new_root)) {
				log_debug("edge %s %s\n",
					  edge.from->id.c_str(), edge.to->id.c_str());
				log_assert(!import_nodes.count(edge.to));
				populate(edge.from);
				populate(edge.to);
				ImportNode::link(import_nodes[edge.from],
								 import_nodes[edge.to], edge);
			}

			log_debug("Linking up node (%s) and new subtree root (%s)\n",
					  node->id.c_str(), new_root->id.c_str());

			// Link up the node of interest and the new subtree root
			for (const auto &edge : links.walk_spantree(node, new_root)) {
				log_debug("edge %s %s\n",
					  edge.from->id.c_str(), edge.to->id.c_str());
				populate(edge.from);
				populate(edge.to);
				ImportNode::link(import_nodes[edge.from],
								 import_nodes[edge.to], edge);
			}
		}

		return import_nodes.at(node);
	}
};

struct ImportsPass : Pass {
	ImportsPass() : Pass("imports", "implement imports") {}
	void execute(std::vector<std::string> args, RTLIL::Design *d) override
	{
		log_header(d, "Executing IMPORTS pass.\n");

		size_t argidx;
		for (argidx = 1; argidx < args.size(); argidx++) {
			break;
		}
		extra_args(args, argidx, d);

		for (auto m : d->selected_modules()) {
			log_debug("Visiting module %s.\n", log_id(m->name));
			Immutlinks links;
			links.parse(m);
			links.index();
			links.module = m;
			SigMap sigmap(m);
			InvertMap imap(m, sigmap);
			links.imap = &imap;

			std::vector<Cell *> import_cells;
			for (auto cell : m->selected_cells())
			if (cell->type == ID(IMPORT))
				import_cells.push_back(cell);

			for (auto cell : import_cells) {
				int width = cell->getParam(ID::WIDTH).as_int();
				Namespace ns = cell_namespace(cell);
				std::string varname = cell->name.str();
				bool zeroed = cell->getParam(ID(ZEROED)).as_bool();
				Immutnode *import_from = cell_immutnode(cell, links, ID(FROM_NODE));
				Immutnode *import_to = cell_immutnode(cell, links, ID(TO_NODE));

				std::string stalk = cell->getParam(ID(STALK)).decode_string();

				if (stalk == "*") {
					::hashlib::dict<Immutnode*, ImportNode*> import_nodes;
					for (auto node : links.nodes)
					if (node->in_namespace(ns))
						import_nodes[node] = new ImportNode(links, node, width, varname);

					import_nodes[import_from]->inject(SigBit(State::S1), cell->getPort(ID::D));

					for (auto node : links.nodes)
					if (node->in_namespace(ns))
					for (auto const &edge : links.edges[node])
					if (edge.dir && edge.in_namespace(ns))
						ImportNode::link(import_nodes[edge.from], import_nodes[edge.to], edge);
				
					for (auto pair : import_nodes)
						pair.second->build(&imap);

					SigSpec imported = import_nodes[import_to]->evaluate_data(zeroed);
					m->connect(cell->getPort(ID::Q), imported);
					m->remove(cell);

					for (auto pair : import_nodes)
						delete pair.second;

					continue;
				}

				std::vector<Immutedge> repre;
				std::vector<std::vector<Immutedge>> smear;

				auto pos = stalk.find_first_of(";");
				repre = parse_fg_element(links, stalk.substr(0, pos));
				if (pos != std::string::npos)
					stalk = stalk.substr(pos + 1);
				else
					stalk = "";

				while (!stalk.empty()) {
					auto pos = stalk.find_first_of(";");
					smear.push_back(parse_fg_element(links, stalk.substr(0, pos)));
					if (pos != std::string::npos)
						stalk = stalk.substr(pos + 1);
					else
						stalk = "";					
				}

				QuotientIndex index;
				for (const auto &edges: smear) {
					int p = QuotientIndex::IDENTITY;
					for (const auto &edge: edges)
						p = index.cross(p, edge.fg_element);
					index.merge(QuotientIndex::IDENTITY, p);
				}

				::hashlib::dict<int, ImportTree *> import_trees;
				ImportTree *tree1;
				ImportTree *tree2;
				import_trees[QuotientIndex::IDENTITY] \
					= tree1 = new ImportTree(links, width, varname);

				for (const auto &edges: smear) {
					int p = QuotientIndex::IDENTITY;
					for (const auto &edge: edges) {
						int next_p = index.cross(p, edge.fg_element);
						if (!import_trees.count(next_p))
							import_trees[next_p] \
								= new ImportTree(links, width, varname);
						ImportNode::link((*import_trees[p])[edge.from],
										 (*import_trees[next_p])[edge.to], edge);
						p = next_p;
					}	
				}

				{
					int p = QuotientIndex::IDENTITY;
					for (const auto &edge: repre) {
						int next_p = index.cross(p, edge.fg_element);
						if (!import_trees.count(next_p))
							import_trees[next_p] \
								= new ImportTree(links, width, varname);
						ImportNode::link((*import_trees[p])[edge.from],
										 (*import_trees[next_p])[edge.to], edge);
						p = next_p;
					}
					tree2 = import_trees[p];
				}

				(*tree1)[import_from]->inject(SigBit(State::S1), cell->getPort(ID::D));

				// Populate the final import node and the links leading to it
				// in time before we ask for the trees to be built
				(*tree2)[import_to];

				for (auto tree : import_trees)
					tree.second->build(&imap);

				SigSpec imported = (*tree2)[import_to]->evaluate_data(zeroed);
				m->connect(cell->getPort(ID::Q), imported);
				m->remove(cell);

				for (auto tree : import_trees)
					delete tree.second;
			}
		}
	}
} ImportsPass;
