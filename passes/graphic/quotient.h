// The Fold compiler
// Copyright 2023 Martin Povišer <povik@cutebit.org>
// Distributed under the terms of the ISC license, see LICENSE

#include <queue>

struct QuotientIndex {
	int next = 1;
	Yosys::dict<int, std::map<Element, int>> table;
	Yosys::dict<int, int> renames;

	static const int IDENTITY = 0;

	int alloc()
	{
		int ret = next;
		next++;
		return ret;
	}

	int canonical(int point)
	{
		while (renames.count(point))
			point = renames.at(point);
		return point;
	}

	int cross(int point, Element groupele)
	{
		log_assert(!renames.count(point));
		std::map<Element, int> &neighbours = table[point];

		if (!neighbours.count(groupele)) {
			int new_point = alloc();
			neighbours[groupele] = new_point;
			table[new_point][groupele.inversed()] = point;
			return new_point;
		}
		return canonical(neighbours.at(groupele));
	}

	int walk(std::vector<Element> path, int start=0)
	{
		int point = start;
		for (auto ele : path)
			point = cross(point, ele);
		return point;
	}

	void merge(int lhs, int rhs)
	{
		std::queue<std::pair<int, int>> qu;
		qu.push(std::make_pair(lhs, rhs));

		while (!qu.empty()) {
			int a, b;
			std::tie(a, b) = qu.front(); qu.pop();
			a = canonical(a);
			b = canonical(b);
			if (a == b)
				continue;
			if (b == IDENTITY)
				std::swap(a, b); // don't rename identity
			std::map<Element, int> &an = table[a];
			std::map<Element, int> &bn = table[b];
			for (auto const& [g, bp] : bn) {
				if (an.count(g)) {
					if (canonical(an.at(g)) != canonical(bn.at(g)))
						qu.push(std::make_pair(an.at(g), bn.at(g)));
				} else {
					an[g] = bn.at(g);
				}
			}
			renames[b] = a;
			table.erase(b);
		}
	}

	void dump(std::ostream &stream)
	{
		std::vector<Element> mentioned_ele;

		for (auto const& [p, n] : table) 
		for (auto const& [g, np] : n)
			mentioned_ele.push_back(g);

		std::sort(mentioned_ele.begin(), mentioned_ele.end());
		auto last = std::unique(mentioned_ele.begin(), mentioned_ele.end());
		mentioned_ele.erase(last, mentioned_ele.end());

		stream << std::string(12, ' ');
		for (auto g : mentioned_ele)
			stream << std::setw(8) << g.str() << " ";
		stream << "\n";
		stream << std::string(12, ' ');
		for (auto g : mentioned_ele)
			stream << std::string(9, '-');
		stream << "\n";

		for (auto const& [p, n] : table) {
			stream << std::string(4, ' ') << std::setw(8) << p;
			stream << " ";
			for (auto g : mentioned_ele) {
				if (n.count(g))
					stream << std::setw(8) << canonical(n.at(g)) << " ";
				else
					stream << std::string(9, ' ');
			}	
			stream << "\n";
		}
	}

	QuotientIndex() {};
	~QuotientIndex() {};
};

const int QuotientIndex::IDENTITY;
