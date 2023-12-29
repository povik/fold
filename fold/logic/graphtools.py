# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import unittest
from functools import cache

class GraphPropEvaluation:
    def __init__(self):
        self.stack = []
        self.on_stack = set()
        self.labels = dict()
        self.lowlink = dict()
        self.queue = []
        self.children = dict()
        self.to_drop = None
        self.vals = dict()

    def label(self, node):
        if node not in self.labels:
            self.labels[node] = self.lowlink[node] = len(self.labels)

    def visit(self, node):
        self.label(node)
        self.stack.append(node)
        self.on_stack.add(node)

        f, args = node[0], node[1:]
        ret = f.inner(*args)

        assert self.stack.pop() is node
        self.on_stack.remove(node)

        return ret

    def got_children(self, children, reducefunc):
        us = self.stack[-1]
        # `us` have `children`
        self.children[us] = list(children)
        self.queue.append((us, children))

        while self.to_drop is None:
            while True:
                node, children = self.queue[-1]
                if len(children) > 0:
                    break
                self.queue.pop()
                if self.children[node]:
                    self.lowlink[node] = min(min([self.lowlink[n] for n
                                                  in self.children[node]]), self.lowlink[node])
                if self.lowlink[node] == self.labels[node]:
                    self.drop_value = reducefunc([
                        self.vals[n] for n in
                        self.stack[self.stack.index(node):]
                        if n in self.vals])
                    self.to_drop = node
                    break
                else:
                    assert self.queue

            if self.to_drop is not None:
                break

            child = children.pop()
            if child not in self.on_stack:
                ret = self.visit(child)
                if node in self.vals:
                    self.vals[node] = reducefunc([self.vals[node], ret])
                else:
                    self.vals[node] = ret
            else:
                self.lowlink[node] = min(self.lowlink[node],
                                         self.labels[child])

        if self.to_drop is us:
            self.to_drop = None
        return self.drop_value


_gp_stack = []


def graph_property(f):
    def wrapper(*args):
        global _gp_stack
        gpe = GraphPropEvaluation()
        _gp_stack.append(gpe)
        ret = gpe.visit((wrapper, *args))
        _gp_stack.pop()
        return ret
    wrapper.inner = f
    return wrapper


def successor_maximum(children, reducefunc=max):
    '''
    Should always be called within a graph_property
    '''
    global _gp_stack
    assert len(_gp_stack)
    gpe = _gp_stack[-1]
    return gpe.got_children(children, reducefunc)


def early_return(f):
    def wrapper(*args):
        if args in wrapper.cache:
            return wrapper.cache[args]
        return f(*args)
    wrapper.cache = dict()
    def register(arg, ret):
        wrapper.cache[tuple(arg)] = ret
    wrapper.register = register
    return wrapper


def find_sccs(nodes, succ_f):
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
        for succ in succ_f(node):
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
    for node in nodes:
        if node in label:
            continue
        visit(node)
    return sccs


def find_upstream_sccs(seed, succ_f):
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
        for succ in succ_f(node):
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
    visit(seed)
    return sccs


def find_convergent_successor(start_point, succ_f):
    sccs = find_upstream_sccs(start_point, succ_f)
    scc_membership = {node: scc for scc in sccs
                      for node in scc}
    loose_ends = []
    head = scc_membership[start_point]
    visited = set([head])
    trip_wire = None
    trip_loose_ends = []
    trip = []
    while True:
        succ = [scc_membership[succ] for node in head
                for succ in succ_f(node)
                if (succ not in head)]
        if not succ:
            return (None, None)
        loose_ends += succ[1:]
        head = succ[0]
        visited.add(head)
        if trip_wire is not None and head in trip_wire:
            visited.update(set(trip[:trip.index(head) + 1]))
            loose_ends += [end for l in trip_loose_ends[:trip.index(head) + 1]
                           for end in l]
            trip_wire = None
        while trip_wire is None:
            loose_ends = [end for end in loose_ends if end not in visited]
            if not loose_ends:
                if len(head) != 1:
                    break # no SCCs
                return list(head)[0], \
                       set([n for scc in visited for n in scc])
            trip_head = loose_ends.pop(0)
            trip = [trip_head]
            trip_loose_ends = [[]]
            while True:
                if trip_head in visited:
                    visited.update(set(trip))
                    loose_ends += [end for l in trip_loose_ends
                                       for end in l]
                    break
                succ = [scc_membership[succ] for node in trip_head
                        for succ in succ_f(node)
                        if succ not in trip_head]
                if not succ:
                    trip_wire = set(trip)
                    break
                trip_head = succ[0]
                trip.append(trip_head)
                trip_loose_ends.append(succ[1:])


class TestGraphtools(unittest.TestCase):
    def test_graph_prop(self):
        endpoints = {
            1: 10,
            2: 20,
            3: 30,
            4: 40,
        }

        children = {
            5: [6, 7],
            6: [1],
            7: [8, 2],
            8: [5, 3, 4],
        }

        expected = {
            1: 10,
            2: 20,
            3: 30,
            4: 40,
            5: 40,
            6: 10,
            7: 40,
            8: 40,
        }

        @graph_property
        def test(a):
            if a in endpoints:
                ret = endpoints[a]
            else:
                ret = successor_maximum([(test, m) for m in children[a]], max)
            self.assertEqual(ret, expected[a])
            return ret

        for n in expected.keys():
            test(n)

    def test_graph_prop_w_cache(self):
        endpoints = {
            1: 10,
            2: 20,
            3: 30,
            4: 40,
        }

        children = {
            5: [6, 7],
            6: [1],
            7: [8, 2],
            8: [5, 3, 4],
        }

        expected = {
            1: 10,
            2: 20,
            3: 30,
            4: 40,
            5: 40,
            6: 10,
            7: 40,
            8: 40,
        }

        @graph_property
        @cache
        def test(a):
            if a in endpoints:
                ret = endpoints[a]
            else:
                ret = successor_maximum([(test, m) for m in children[a]], max)
            self.assertEqual(ret, expected[a])
            return ret

        for n in expected.keys():
            test(n)


def main():
    unittest.main()


if __name__ == "__main__":
    main()
