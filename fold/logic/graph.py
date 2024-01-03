# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import functools
from functools import cache
import copy


class BaseGraph:
    def __init__(self):
        self.nodes = set()
        self.edges = []
        self.edge_clusters = dict()
        self.cluster_parent = dict()

    @property
    def bare_edges(self):
        for edge in self.edges:
            yield (edge.ep1, edge.ep2)


class Edge:
    EP1_PROPS = ()
    EP2_PROPS = ()

    def __init__(self, ep1, ep2):
        self.ep1, self.ep2 = ep1, ep2
        self._canon_rev = None

    @property
    def tail(self):
        return self.ep1

    @property
    def head(self):
        return self.ep2

    @property
    def rev(self):
        if self._canon_rev is None:
            rev = copy.copy(self)
            rev.ep1, rev.ep2 = self.ep2, self.ep1
            for prop1, prop2 in zip(self.EP1_PROPS, self.EP2_PROPS):
                val1, val2 = getattr(rev, prop1), getattr(rev, prop2)
                setattr(rev, prop1, val2); setattr(rev, prop2, val1)
            rev._canon_rev = self
            self._canon_rev = rev
        return self._canon_rev

    @property
    def back_n_forth(self):
        return [self, self.rev]


class SpanningTreeIndex:
    def __init__(self, by_index, by_node):
        self.by_index, self.by_node = by_index, by_node

    @classmethod
    def _common_index(self, ai, bi):
        common = []
        for a, b in zip(ai, bi):
            if a != b:
                break
            common.append(a)
        return common

    def common(self, *args):
        assert len(args) >= 1
        idx = functools.reduce(self._common_index, [self.by_node[node] for node in args])
        return self.by_index[tuple(idx)][0]

    @classmethod
    def form_path(self, ai, bi):
        common = self._common_index(ai, bi)

        curr = ai
        while len(curr) > len(common):
            yield (curr, False)
            curr = curr[:-1]

        while len(curr) < len(bi):
            curr = curr + (bi[len(curr)],)
            yield (curr, True)

    def _walk(self, a, b):
        for idx, down in self.form_path(self.by_node[a], self.by_node[b]):
            node, edge = self.by_index[idx]
            if down:
                edge = edge.rev
            yield edge

    def walk(self, a, b):
        curr = a
        for edge in self._walk(a, b):
            assert edge.tail == curr
            curr = edge.head
            yield edge
        assert curr == b


class SpanningTree:
    def __init__(self, node=None):
        self.edges = set()
        self.nodes = set([node] if node is not None
                         else [])
        self.finalized = False

    def merge(self, other, edge):
        assert not self.finalized

        ret = SpanningTree()
        ret.nodes = self.nodes.union(other.nodes)
        ret.edges = self.edges.union(other.edges)
        ret.edges.add(edge)
        return ret

    @classmethod
    def _walk_tree(self, root, node_edges, ignore_edge=None):
        yield ((), root, ignore_edge)

        index = 0
        for edge in node_edges[root]:
            if edge is ignore_edge:
                continue

            assert edge.rev.rev is edge

            for subindices, vnode, vedge \
                    in self._walk_tree(edge.head, node_edges, edge.rev):
                yield ((index,) + subindices, vnode, vedge)

            index += 1

    @classmethod
    def walk_tree(self, root, edges):
        node_edges = dict()

        for edge in edges:
            for edgeway in edge.back_n_forth:
                if edgeway.tail not in node_edges:
                    node_edges[edgeway.tail] = []
                node_edges[edgeway.tail].append(edgeway)

        yield from self._walk_tree(root, node_edges)

    def finalize(self):
        self.finalized = True

    @property
    @cache
    def index(self):
        return self.build_index()

    def build_index(self):
        assert self.finalized

        # pick one node to be the head
        head = list(self.nodes)[0]
        by_index, by_node = dict(), dict()
        for i, node, edge in self.walk_tree(head, self.edges):
            by_index[i] = (node, edge)
            by_node[node] = i
        return SpanningTreeIndex(by_index, by_node)


class Graph(BaseGraph):
    def __init__(self):
        BaseGraph.__init__(self)
        self._ep_edges = dict()
        self._spans = []
        self._subgraphs = []
        self._link_fg_eles = dict()

    def walk_eps(self, ep): # TODO: rename
        assert ep in self.nodes
        return self._ep_edges.get(ep, [])

    def walk_eps_rev(self, ep): # TODO: rename
        assert ep in self.nodes
        return [edge.rev for edge in self._ep_edges.get(ep, [])]

    @property
    def root(self):
        return self

    @property
    def biedges(self):
        for edge in self.edges:
            yield edge
            yield edge.rev

    def add_node(self, node):
        assert node not in self.nodes
        self.nodes.add(node)
        self._spans.append(SpanningTree(node))

    def _find_span(self, node):
        for s in self._spans:
            if node in s.nodes:
                return s
        assert False

    def _remove_span(self, span):
        idx = self._spans.index(span)
        del self._spans[idx]

    def _infer_fg_ele(self, edge, forceelem=False):
        if forceelem:
            return EdgeElement(edge)

        ep1, ep2 = edge.head, edge.tail
        s1, s2 = self._find_span(ep1), self._find_span(ep2)
        if s1 != s2:
            # We need to merge the spanning trees,
            # this edge does not close a loop.
            self._remove_span(s1)
            self._remove_span(s2)
            self._spans.append(s1.merge(s2, edge))
            return Identity()
        else:
            return EdgeElement(edge)

    def finalize(self):
        for span in self._spans:
            span.finalize()

    def link(self, edge, forceelem=False):
        ep1, ep2 = edge.head, edge.tail
        assert ep1 in self.nodes and ep2 in self.nodes
        self.edges.append(edge)
        if ep1 not in self._ep_edges:
            self._ep_edges[ep1] = []
        self._ep_edges[ep1].append(edge)
        if ep2 not in self._ep_edges:
            self._ep_edges[ep2] = []
        self._ep_edges[ep2].append(edge.rev)

        ele = self._infer_fg_ele(edge, forceelem=forceelem)
        self._link_fg_eles[edge] = ele
        self._link_fg_eles[edge.rev] = ele.inv
        return ele

    def __iadd__(self, edge):
        self.link(edge)
        return self

    def add_subgraph(self):
        ret = Subgraph(self, self)
        self._subgraphs.append(ret)
        return ret

    @classmethod
    def _dump_nodes_n_edges(self, nodes, edges, printed, topgraph, f):
        for node in nodes:
            if node in printed:
                continue
            printed.add(node)
            print("\t%s [label=\"%s\" shape=box3d fillcolor=gray style=filled];"
                  % (id(node), node.label.replace("'", "\\\"")), file=f)

        for edge in edges:
            if edge in printed:
                continue
            printed.add(edge)
            ele = topgraph._link_fg_eles[edge]
            labels = ([] if ele == Identity() else [f"{ele}"]) \
                     + edge.internal_labels
            attrs = [
                "color=black",
                f"label=<{', '.join(labels)}>",
            ] + (["dir=none"] if (ele == Identity()) else [])

            print(f"\t{id(edge.ep1)} -> {id(edge.ep2)} [{', '.join(attrs)}];", file=f)

        return printed

    def _dump_content(self, printed, f):
        for subgraph in self._subgraphs:
            printed = subgraph._dump_content(printed, f)
        printed = self._dump_nodes_n_edges(self.nodes,
                                    self.edges, printed, self, f)
        return printed

    def dump(self, f):
        print("digraph G {", file=f)
        self._dump_content(set(), f)
        print("}", file=f)


class Subgraph(Graph):
    def __init__(self, parent, topgraph):
        self.parent = parent
        self.nodes = set()
        self.edges = []
        self.topgraph = topgraph
        self._spans = []
        self._subgraphs = list()
        self._ep_edges = dict()

    @property
    def root(self):
        graph = self
        while isinstance(graph, Subgraph):
            graph = graph.parent
        return graph

    def add_subgraph(self):
        ret = Subgraph(self, self.topgraph)
        self._subgraphs.append(ret)
        return ret

    def add_node(self, node):
        assert node not in self.nodes
        self.nodes.add(node)
        self.parent.add_node(node)
        self._spans.append(SpanningTree(node))

    def _find_span(self, node):
        for s in self._spans:
            if node in s.nodes:
                return s
        assert False

    def _remove_span(self, span):
        idx = self._spans.index(span)
        del self._spans[idx]

    def _merge_spans(self, edge):
        ep1, ep2 = edge.head, edge.tail
        s1, s2 = self._find_span(ep1), self._find_span(ep2)
        if s1 != s2:
            # We need to merge the spanning trees,
            # this edge does not close a loop.
            self._remove_span(s1)
            self._remove_span(s2)
            self._spans.append(s1.merge(s2, edge))

    def link(self, edge, forceelem=False):
        ep1, ep2 = edge.head, edge.tail
        assert ep1 in self.nodes and ep2 in self.nodes
        if ep1 not in self._ep_edges:
            self._ep_edges[ep1] = []
        self._ep_edges[ep1].append(edge)
        if ep2 not in self._ep_edges:
            self._ep_edges[ep2] = []
        self._ep_edges[ep2].append(edge.rev)
        self.edges.append(edge)
        if not forceelem:
            self._merge_spans(edge)
        return self.parent.link(edge, forceelem=forceelem)

    def __iadd__(self, edge):
        self.link(edge)

    def _dump_content(self, printed, f):
        print("subgraph cluster_%d {" % id(self), file=f)
        for subgraph in self._subgraphs:
            printed = subgraph._dump_content(printed, f)
        printed = Graph._dump_nodes_n_edges(self.nodes,
                                    self.edges, printed, self.topgraph, f)
        print("}", file=f)
        return printed


class DirectedGraph(BaseGraph):
    def __init__(self):
        BaseGraph.__init__(self)
        self._subgraphs = []
        self._tail_edges = dict()
        self._head_edges = dict()
        self._tailhead_edges = dict()

    def walk_tails(self, tail):
        assert tail in self.nodes
        return self._tail_edges.get(tail, [])

    def walk_heads(self, head):
        assert head in self.nodes
        return self._head_edges.get(head, [])

    def walk_tailheads(self, tail, head):
        assert tail in self.nodes and head in self.nodes
        return self._tailhead_edges.get((tail, head), [])

    def add_node(self, node):
        assert node not in self.nodes
        self.nodes.add(node)

    def link(self, edge):
        tail, head = edge.tail, edge.head
        assert tail in self.nodes and head in self.nodes
        self.edges.append(edge)
        if tail not in self._tail_edges:
            self._tail_edges[tail] = []
        self._tail_edges[tail].append(edge)
        if head not in self._head_edges:
            self._head_edges[head] = []
        self._head_edges[head].append(edge)
        if (tail, head) not in self._tailhead_edges:
            self._tailhead_edges[(tail, head)] = []
        self._tailhead_edges[(tail, head)].append(edge)

    def __iadd__(self, edge):
        self.link(edge)
        return self

    @classmethod
    def _dump_nodes_n_edges(self, nodes, edges, printed, topgraph, f):
        for node in nodes:
            if node in printed:
                continue
            printed.add(node)
            print("\t%s [label=\"%s\" shape=box3d fillcolor=gray style=filled];" % (id(node), node.label), file=f)

        for edge in edges:
            if edge in printed:
                continue
            printed.add(edge)
            attrs = [
                # dir=none, color={color}, label=\"{label}\"
                "color=black",
                f"label=\"{edge.dump_attrs()} {str(edge.imprint_push)} {str(edge.imprint_pop)}\"",
            ]
            print(f"\t{id(edge.ep1)} -> {id(edge.ep2)} [{', '.join(attrs)}];", file=f)

        return printed

    def _dump_content(self, printed, f):
        #for subgraph in self._subgraphs:
        #   printed = subgraph._dump_content(printed, f)
        printed = self._dump_nodes_n_edges(self.nodes,
                                    self.edges, printed, self, f)
        return printed

    def dump(self, f):
        print("digraph G {", file=f)
        self._dump_content(set(), f)
        print("}", file=f)

    def find_sccs(self):
        ret = []
        label = dict()
        lowlink = dict()
        stack = []
        on_stack = set()
        sccs = []

        def visit(node):
            lowlink[node] = label[node] = len(label)
            stack.append(node)
            on_stack.add(node)
            for edge in self.walk_tails(node):
                succ = edge.head
                if succ not in label:
                    visit(succ)
                    lowlink[node] = min(lowlink[node], lowlink[succ])
                elif succ in on_stack:
                    lowlink[node] = min(lowlink[node], label[succ])

            if lowlink[node] == label[node]:
                scc_nodes = []
                while True:
                    pop = stack.pop()
                    on_stack.remove(pop)
                    scc_nodes.append(pop)
                    if pop is node:
                        break
                sccs.append(frozenset(scc_nodes))

        for node in self.nodes:
            if node in label:
                continue
            visit(node)

        return sccs


class GroupElement:
    @classmethod
    def free_cast(self, elem):
        if type(elem) is Identity:
            return []
        elif type(elem) is FreeElement:
            return elem.constituents
        elif isinstance(elem, GroupElement):
            return [elem]
        else:
            raise NotImplementedError(type(elem))

    def __mul__(self, other):
        try:
            A = self.free_cast(self)
            B = self.free_cast(other)
        except NotImplementedError:
            return NotImplemented
        ret = A + B
        pos = 0
        while pos < len(ret) - 1:
            if ret[pos] == ret[pos + 1].inv:
                del ret[pos]
                del ret[pos]
                pos = max(pos - 1, 0)
                continue
            pos += 1
        if len(ret) == 0:
            return Identity()
        elif len(ret) == 1:
            return ret[0]
        else:
            return FreeElement(ret)

    def __str__(self):
        return " ".join(str(e) for e in self.constituents)


class Identity(GroupElement):
    def __eq__(self, other):
        return type(other) is Identity

    def __hash__(self):
        return hash(self.__class__)

    def __str__(self):
        return "(id)"

    @property
    def inv(self):
        return self


class EdgeElement(GroupElement):
    IDX = 0
    LABELS = "fghijklmnopqrstuvwxyz0123456789abcdeABCDEFGHIJKLMNOPQRSTUVWXYZ"

    def __init__(self, edge, label=None):
        self.edge = edge
        if label is None:
            if EdgeElement.IDX < len(EdgeElement.LABELS):
                self.label = EdgeElement.LABELS[EdgeElement.IDX]
            else:
                self.label = f"${EdgeElement.IDX}"
            EdgeElement.IDX += 1
        else:
            self.label = label

    def __eq__(self, other):
        return type(other) is EdgeElement \
            and other.edge == self.edge

    def __hash__(self):
        return hash(self.edge)

    def __str__(self):
        return f"{self.label}"

    @property
    def inv(self):
        return EdgeElement(self.edge.rev, label=(f"{self.label}^-1" if
                           not self.label.endswith("^-1") else self.label[:-3]))


class FreeElement(GroupElement):
    def __init__(self, constituents):
        self.constituents = constituents

    @property
    def inv(self):
        return FreeElement(list(reversed([e.inv for e in self.constituents])))


class Subgroup:
    '''Subgroup which is described by a small number of generator elements'''
    def __init__(self, gens=set()):
        self.gens = gens

    def __mul__(self, other):
        return Subgroup(self.gens + other.gens)

    @property
    def inv(self):
        return Subgroup(set([ele.inv for ele in self.gens]))


class SmearedGroupele:
    def __init__(self, base, smear):
        self.base = base
        self.smear = smear
        assert isinstance(self.base, GroupElement)
        assert all(isinstance(x, GroupElement) for x in smear)

    @classmethod
    def of_subgraph(self, a, b, subgraph, mask=None):
        '''
        Walk the given subgraph from a to b and build up
        the smeared group element (a subset of the
        fundamental group of the full graph).
        '''

        if mask is None:
            mask = lambda e: True

        visited = dict()
        visited[a] = Identity()
        done = set()
        queue = [a]
        smear = []

        while len(queue):
            node = queue.pop(0)

            for edge in subgraph.walk_eps_rev(node):
                if edge in done or not mask(edge):
                    continue

                edge_ele = subgraph.root._link_fg_eles[edge]

                if edge.head in visited:
                    smear.append(visited[node] * edge_ele *
                                 visited[edge.head].inv)
                else:
                    visited[edge.head] = visited[node] * edge_ele
                    queue.append(edge.head)

                done.add(edge)
                done.add(edge.rev)

        if b not in visited:
            raise ValueError("b not reachable from a")

        if not len(smear):
            return visited[b]
        else:
            return SmearedGroupele(visited[b], smear)

    @property
    def inv(self):
        return SmearedGroupele(self.base.inv,
                [self.base.inv * s.inv * self.base for s in self.smear])

    def __mul__(self, other):
        if isinstance(other, GroupElement):
            return SmearedGroupele(self.base * other, self.smear)
        elif isinstance(other, SmearedGroupele):
            return SmearedGroupele(self.base * other.base, self.smear +
                                    [self.base * s * self.base.inv for s in other.smear])
        else:
            raise NotImplementedError(type(other))

    def __rmul__(self, other):
        if isinstance(other, GroupElement):
            return SmearedGroupele(other * self.base, [other * s * other.inv for s in self.smear])
        elif isinstance(other, SmearedGroupele):
            return SmearedGroupele(other.base * self.base, other.smear +
                                    [other.base * s * other.base.inv for s in self.smear])
        else:
            raise NotImplementedError(type(other))

    def __str__(self):
        smear_str = "; ".join(str(s) for s in self.smear)
        return f"({self.base} [{smear_str}])"
