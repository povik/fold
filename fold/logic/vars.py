# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from functools import cache

from ..ast import BadInput, markers_str, Tuple
from ..utils import hint, hint_pre, product, mrange, log2ceil, \
                    encode_multiindex

from .shape import Shape, SignalValue, Value
from .graph import DirectedGraph, Edge, Identity, SmearedGroupele, \
                   GroupElement
from . import rtl, graphtools, execid
from .graphtools import graph_property, successor_maximum


class BaseVarImpl:
    def __init__(self, f, varname, shape):
        assert isinstance(shape, Shape)
        self.f = f
        self.varname = varname
        self.shape = shape
        self.m = f.design.rtl_module
        self.verbose = False


class VarImpl(BaseVarImpl):
    def __init__(self, *args):
        super().__init__(*args)
        self.assigns = dict()
        self.evals = dict()

    def assign(self, bi, val, *addr):
        if len(addr) != 0:
            raise BadInput("bad addressing") # TODO
        self.assigns[bi] = val.cast(self.shape).extract_underlying_signal()

    def eval(self, bi, *args):
        if len(args):
            raise NotImplementedError(args)

        if bi in self.assigns:
            return SignalValue(self.assigns[bi], self.shape)

        if bi not in self.evals:
            self.evals[bi] = self.f.design.rtl_module \
                                .add_wire(f"\\{self.varname}", self.shape.bitlen)
        return SignalValue(self.evals[bi], self.shape)


class VarImplSignal(VarImpl):
    def __init__(self, *args):
        super().__init__(*args)
        self.routers = dict()
        self.injects = []

    def inject_arg(self, bi, en, val):
        self.injects.append((bi, en, val))

    def build(self):
        for bi, en, val in self.injects:
            sig = val.cast(self.shape).extract_underlying_signal()
            with rtl.SynthAttrContext(keep=True):
                self.m.add_cell("\\VAR_SET",
                    ("\\D", sig),
                    ("\\EN", en),
                    WIDTH=sig.width,
                    AT_NODE=bi.id,
                    NAMESPACE=self.f.namespace,
                    NAME=self.varname,
                )

        for bi, sig in self.assigns.items():
            with rtl.SynthAttrContext(keep=True):
                self.m.add_cell("\\VAR_SET",
                    ("\\D", sig),
                    ("\\EN", rtl.HIGH),
                    WIDTH=sig.width,
                    AT_NODE=bi.id,
                    NAMESPACE=self.f.namespace,
                    NAME=self.varname,
                )

        for bi, sig in self.evals.items():
            self.m.add_cell("\\VAR_GET",
                ("\\Q", sig),
                WIDTH=sig.width,
                AT_NODE=bi.id,
                NAMESPACE=self.f.namespace,
                NAME=self.varname,
            )


class MutLink(Edge):
    EP1_PROPS = ('stalk', 'hot')

    MATCH_NONE = 1
    MATCH_ANY = 2

    def __init__(self, tail, head, stalk, hot, imprint_push=None, imprint_pop=None):
        super().__init__(tail, head)
        self.stalk = stalk
        self.hot = hot
        self.imprint_push = [imprint_push] if imprint_push is not None else []
        self.imprint_pop = [imprint_pop] if imprint_pop is not None else []

    def dump_attrs(self):
        if self.stalk != Identity():
            return str(self.stalk)
        else:
            return ""

    def __str__(self):
        return f"mutlink({self.ep1} -> {self.ep2})"

    @classmethod
    def imprint_step_matches(self, a, b):
        if a == self.MATCH_ANY or b == self.MATCH_ANY:
            return True
        if a == self.MATCH_NONE or b == self.MATCH_NONE:
            return False
        return a == b


def escape_stalk(stalk):
    if isinstance(stalk, GroupElement):
        repre, smear = stalk, []
    else:
        repre, smear = stalk.base, stalk.smear

    groupele_str = lambda g: "".join([atom.label for atom
                                      in GroupElement.free_cast(g)])

    return f"{groupele_str(repre)};" + ";" .join(groupele_str(g) for g in smear)


class VarImplMutableGlobal(VarImpl):
    def __init__(self, *args):
        super().__init__(*args)
        self.injects = []

    def inject_arg(self, bi, en, val):
        self.injects.append((bi, en, val))

    def eval(self, bi, *args):
        if len(args):
            raise NotImplementedError(args)

        if bi in self.assigns:
            raise BadInput("read-after-write within the same cycle on global-register implemented variable {:h}",
                           self.varname)

        if bi not in self.evals:
            self.evals[bi] = self.f.design.rtl_module \
                                .add_wire(f"\\{self.varname}", self.shape.bitlen)
        return SignalValue(self.evals[bi], self.shape)

    def build(self):
        ei_width = self.f.design.execid_width
        m = self.f.design.rtl_module
        clk, rst = self.f.design.rtl_clk, self.f.design.rtl_rst

        cases = [
            (en, val.cast(self.shape).extract_underlying_signal())
            for bi, en, val in self.injects
        ] + [
            (bi.en, val) for bi, val in self.assigns.items()
        ]

        val, hot = rtl.build_mux(m, cases, width=self.shape.bitlen)

        with rtl.SynthAttrContext(fold_debug=True):
            m.add_cell_keep(
                "\\MUTEX_ASSERT",
                ("\\CLK", self.f.d.rtl_clk),
                ("\\A", rtl.concat(*[en for en, _ in cases])),
                WIDTH=len(cases),
                MESSAGE=f"write to {self.varname!s}",
            )

        val_q = rtl.DFF(m, clk, val, en=hot)

        for bi, sig in self.evals.items():
            m.connect(sig, val_q)

        frame_execid, _ = rtl.build_mux(m, [
            (en, self.f.frame_execid(bi)) for bi, en, _ in self.injects
        ] + [
            (bi.en, self.f.frame_execid(bi)) for bi, val in self.assigns.items()
        ], width=ei_width)

        frame_execid_q = rtl.DFF(
            m, clk, rtl.concat(rtl.HIGH, frame_execid), en=hot,
            rst_value=rtl.concat(rtl.LOW, rtl.repeat(rtl.UNDEF, ei_width)),
            rst=rst
        )

        frame_execid_kept = rtl.OR(m, rtl.NOT(m, hot), execid.EQ(m,
            frame_execid_q[1:],
            frame_execid,
            rtl.AND(m, frame_execid_q[0:1], hot)
        ))

        with rtl.SynthAttrContext(execid_assert=True):
            for bi, sig in self.evals.items():
                rtl.ASSERT(m, bi.en, execid.EQ(m,
                    frame_execid_q[1:],
                    self.f.frame_execid(bi),
                    rtl.AND(m, bi.en, frame_execid_q[0:1])
                ))

        write_execid, _ = rtl.build_mux(m, [
            (en, bi.execid) for bi, en, _ in self.injects
        ] + [
            (bi.en, bi.execid) for bi, _ in self.assigns.items()
        ], width=ei_width)

        write_execid_q = rtl.DFF(m, clk, write_execid, en=hot)

        with rtl.SynthAttrContext(execid_assert=True):
            for bi, sig in self.evals.items():
                check_en = rtl.AND(m, bi.en, frame_execid_q[0:1])

                rtl.ASSERT(m, check_en, execid.GE(m,
                    bi.execid,
                    write_execid_q,
                    check_en
                ))

        read_execid_q = m.add_wire(f"${self.varname}$read_execid_q", ei_width + 1)

        read_execid, read_execid_valid = execid.MAX(m, [
            (bi.en, bi.execid) for bi, _ in self.evals.items()
        ] + [
            (rtl.AND(m, write_execid_q[0:1], frame_execid_kept), read_execid_q[1:])
        ], y_width=ei_width)

        m.connect(read_execid_q,
            rtl.DFF(m, clk, rtl.concat(read_execid_valid, read_execid)))

        with rtl.SynthAttrContext(execid_assert=True):
            for bi, sig in self.assigns.items():
                check_en = rtl.AND(m, bi.en, read_execid[0:1])
                rtl.ASSERT(m, check_en, execid.GE(m,
                    bi.execid,
                    read_execid,
                    check_en
                ))

class VarImplMutable(VarImpl):
    def __init__(self, *args):
        super().__init__(*args)

    def _mux_import(self, edge):
        if edge.tail in self.assigns:
            upstream_sig = self.assigns[edge.tail]
        else:
            upstream_sig, _ = self._import(edge.tail)

        assert upstream_sig.width == self.shape.bitlen
        bridged = self.m.add_wire(f"\\{self.varname}", self.shape.bitlen)
        with rtl.SynthAttrContext(implements_variable=self.varname):
            self.m.add_cell("\\IMPORT",
                ("\\D", upstream_sig),
                ("\\Q", bridged),
                WIDTH=bridged.width,
                ZEROED=False,
                NAMESPACE=self.f.namespace,
                FROM_NODE=edge.tail.id,
                TO_NODE=edge.head.id,
                STALK=escape_stalk(edge.stalk),
            )
        return edge.hot, bridged

    def _build_mux(self, bi):
        cases = [
            self._mux_import(inward)
            for inward in self.f.mutlinks.walk_heads(bi)
        ]
        src = f"forwarding variable '{self.varname}'" \
              + (f" at {bi.src}" if bi.src != "" else "")
        with rtl.SynthAttrContext(src=src):
            return rtl.build_mux(self.m, cases, width=self.shape.bitlen)

    @graphtools.early_return
    def _import(self, bi):
        sig = self.m.add_wire(f"$import${self.varname}", self.shape.bitlen,
                              implements_variable=self.varname,
                              for_scope=str(bi))
        en = self.m.add_wire(f"$import${self.varname}$en", 1,
                             implements_variable=self.varname,
                             for_scope=str(bi))
        VarImplMutable._import.register((self, bi), (sig, en))

        if self.verbose:
            print(f"Importing to {bi}")

        if (ret := self._attempt_simple_import(bi)) is not None:
            sig_, en_ = ret
        else:
            sig_, en_ = self._build_mux(bi)

        self.m.connect(sig, sig_)
        self.m.connect(en, en_)
        return sig, en

    def assign(self, bi, val, *addrs):
        if len(addrs) == 0:
            self.assigns[bi] = val.cast(self.shape).extract_underlying_signal()
            return

        m = self.m
        innershape = self.shape.drop_dims(len(addrs))

        oldval = self.eval(bi)

        # TODO: check logic
        off = rtl.Signal.from_const(0, 1)
        for a, s in zip(addrs, reversed(self.shape.dims)):
            off = rtl.ADD(m,
                rtl.MUL(m, off, rtl.Signal.from_const(s, s.bit_length())),
                a.extract_signal()
            )
        off = rtl.MUL(m, rtl.Signal.from_const(innershape.bitlen, innershape.bitlen.bit_length()), off)
        mask = rtl.SHL(m, rtl.Signal.from_bits([rtl.BitState.S1] * innershape.bitlen), off,
                       reswidth=self.shape.bitlen)

        assert mask.width == self.shape.bitlen

        self.assigns[bi] = rtl.OR(m,
            rtl.AND(m, oldval.extract_underlying_signal(), rtl.NOT(m, mask)),
            rtl.AND(m, rtl.SHL(m, val.cast(innershape).extract_underlying_signal(),
                               off, reswidth=self.shape.bitlen), mask),
        )

    def build(self):
        for bi, sig in self.evals.items():
            sig_, _ = self._import(bi)
            self.m.connect(sig, sig_)

    @classmethod
    def get_group_cover(self, a, b, domain, succ_f):
        visited = dict()
        visited[a] = Identity()
        queue = [a]
        smear = []

        while len(queue):
            node = queue.pop(0)

            for head, edge in succ_f(node):
                if head not in domain:
                    continue
                edge_ele = edge.stalk.inv

                if head in visited:
                    x = visited[node] * edge_ele \
                                 * visited[head].inv
                    if isinstance(x, GroupElement):
                        smear.append(x)
                    elif isinstance(x, SmearedGroupele):
                        smear.append(x.base)
                        for s in x.smear:
                            smear.append(s * x.base)
                else:
                    visited[head] = visited[node] * edge_ele
                    queue.append(head)

        if b not in visited:
            raise ValueError(f"{b} not reachable from {a} (domain: {domain})")

        for e in smear:
            assert isinstance(e, GroupElement)

        if not len(smear):
            return visited[b].inv
        else:
            if isinstance(visited[b], GroupElement):
                return SmearedGroupele(visited[b], smear).inv
            else:
                return SmearedGroupele(visited[b].base, visited[b].smear + smear).inv

    @classmethod
    def find_assigned_domain(self, seed, succ_f):
        visited = set([seed])
        queue = set([seed])
        preds = dict()
        assigns = set()
        while len(queue):
            head = queue.pop()
            if head[1]:
                assigns.add(head)
            for succ, _ in succ_f(head):
                if succ not in preds:
                    preds[succ] = set()
                preds[succ].add(head)
                if succ not in visited:
                    visited.add(succ)
                    queue.add(succ)

        queue = set(assigns)
        visited = set(assigns)
        while len(queue):
            tail = queue.pop()
            for pred in preds.get(tail, []):
                if pred not in visited:
                    visited.add(pred)
                    queue.add(pred)
        return visited

    def _attempt_simple_import(self, bi):
        curr = (bi, False, ())
        domain = set()

        def succ_f(node):
            immut_node, assigned, stack = node
            if assigned:
                return
            else:
                for edge in self.f.mutlinks.walk_heads(immut_node):
                    if True: # TODO: find a testcase for this
                        stack_copy = list(stack)
                        no_match = False
                        for p in edge.imprint_pop:
                            if not len(stack_copy):
                                continue
                            if not MutLink.imprint_step_matches(stack_copy.pop(-1), p):
                                no_match = True
                                break
                        if no_match:
                            continue
                        for p in edge.imprint_push:
                            stack_copy.append(p)
                    else:
                        stack_copy = []
                    yield ((edge.tail, edge.tail in self.assigns or edge.tail is bi, tuple(stack_copy)), edge)

        assign_domain = self.find_assigned_domain(curr, succ_f)

        while (nextres \
                := graphtools.find_convergent_successor(curr, lambda node: (n[0] for n in succ_f(node) \
                                                                            if n[0] in assign_domain))) \
                                        != (None, None):
            curr = nextres[0]
            domain.update(nextres[1])

        if curr == (bi, False, ()):
            return

        if curr[0] == bi and curr[0] not in self.assigns:
            return rtl.repeat(rtl.UNDEF, self.shape.bitlen), rtl.LOW

        if curr[1]:
            sig, en = self.assigns[curr[0]], rtl.HIGH
        else:
            sig, en = self._import(curr[0])

        gc = self.get_group_cover((bi, False, ()), curr, domain, succ_f)

        if self.verbose:
            print(f"Simple import: {gc} from {curr[0]} to {bi}")

        bridged = self.m.add_wire(f"\\{self.varname}", self.shape.bitlen)
        with rtl.SynthAttrContext(implements_variable=self.varname):
            self.m.add_cell("\\IMPORT",
                ("\\D", sig),
                ("\\Q", bridged),
                WIDTH=bridged.width,
                ZEROED=False,
                NAMESPACE=self.f.namespace,
                FROM_NODE=curr[0].id,
                TO_NODE=bi.id,
                STALK=escape_stalk(gc),
            )
        hot = rtl.OR(self.m, *[edge.hot for edge in self.f.mutlinks.walk_heads(bi)])
        return bridged, hot


class AddressableVarBacking:
    def read(self, bi, en, addr, data, impldata):
        raise NotImplementedError

    def write(self, bi, en, addr, data, impldata):
        raise NotImplementedError

    @property
    def auxdatalen(self):
        return 0


class MemoryVarBacking(AddressableVarBacking):
    def __init__(self, frame, name, shape):
        assert type(shape) is Shape
        self.shape = shape

        self.frame = frame
        self.d = frame.d
        m = self.d.rtl_module

        # TODO
        self.name = f"\\mem_{name}"
        self.shadow_name = f"\\shadow_{name}"

        # Add the memory to be as narrow as possible, possibly to be
        # extended later (outside the frontend) based on access patterns
        attrs = {}
        if hint("ramstyle"):
            attrs["ramstyle"] = hint("ramstyle")
        with rtl.SynthAttrContext(**attrs):
            m.add_memory(self.name, product(*shape.dims[:-1]), shape.finaldim)

        self.shadow_width = 1 + self.d.execid_width
        m.add_memory(self.shadow_name, product(*shape.dims[:-1]), self.shadow_width)

        shadow_initword = rtl.Signal.from_bits([rtl.BitState.S0] \
                                               + [rtl.BitState.Sx] * self.d.execid_width)
        shadow_nwords = product(*shape.dims[:-1])
        rtl.MEMINIT(m, self.shadow_name,
            addr=rtl.Signal.from_bits([]),
            data=rtl.repeat(shadow_initword, shadow_nwords),
            width=self.shadow_width,
        )

    @classmethod
    def _unpack(self, shape, packed_signal, padbit=rtl.BitState.S0):
        padword = rtl.Signal.from_bits([padbit] * shape.finaldim)
        return rtl.concat(*[
            (packed_signal.slot(shape.finaldim, encode_multiindex(j, shape.dims[:-1]))
                if all([a < b for a, b in zip(j, shape.dims)]) else padword)
            for j in mrange(*[2 ** ((s-1).bit_length()) \
                              for s in shape.dims[:-1]])
        ])

    def read(self, bi, en, addr, data, auxdata, transparent=False):
        m = self.d.rtl_module
        assert en.width == 1
        data_unpacked = self._unpack(data.shape, data.extract_underlying_signal(),
                                     padbit=rtl.BitState.Sx)
        wide_log2 = log2ceil(data_unpacked.width // self.shape.finaldim)
        assert self.shape.finaldim * 2**wide_log2 == data_unpacked.width

        rtl.MEMRD(m,
            memid=self.name,
            clk=self.d.rtl_clk,
            **rtl.wrap_in_timeportal2(m,
                en=(en, False),
                addr=(rtl.concat(rtl.Signal.from_bits([rtl.BitState.S0] * wide_log2),
                                 addr), False),
                data=(rtl.SEER(m, rtl.replace_x_bits_w_wire(m, data_unpacked),
                               1, backwards=True), True),
                bank_a_domain=self.name,
                bank_a_constraint="fixed",
            ),
            transparent=transparent,
        )

        shadow_data = m.add_wire("$shadow_data", (1 + self.d.execid_width) * 2**wide_log2)
        rtl.MEMRD(m,
            memid=self.shadow_name,
            clk=self.d.rtl_clk,
            **rtl.wrap_in_timeportal2(m,
                en=(en, False),
                addr=(rtl.concat(rtl.Signal.from_bits([rtl.BitState.S0] * wide_log2),
                                 addr), False),
                data=(rtl.SEER(m, shadow_data,
                               1, backwards=True), True),
                bank_a_domain=self.name,
                bank_a_constraint="fixed",
            ),
            transparent=transparent,
        )

        ei_width = self.d.execid_width
        expected_id = auxdata[:ei_width]
        per_byte = []
        for j in range(2 ** wide_log2):
            base = j * (1 + ei_width)
            eq = execid.EQ(m, expected_id, shadow_data[base + 1:base + 1 + ei_width], en)
            valid = shadow_data[base:base + 1]
            per_byte.append(rtl.AND(m, eq, valid))

        with rtl.SynthAttrContext(execid_assert=True,
                                  guards_memoryvar=self.name):
            rtl.ASSERT(m, en, rtl.AND(m, *per_byte))

    def write(self, bi, en, addr, data, auxdata):
        m = self.d.rtl_module
        assert en.width == 1
        en_wide = self._unpack(data.shape, rtl.repeat(en, data.shape.bitlen),
                               padbit=rtl.BitState.S0)
        data_unpacked = self._unpack(data.shape, data.extract_underlying_signal(),
                                     padbit=rtl.BitState.Sx)
        wide_log2 = log2ceil(data_unpacked.width // self.shape.finaldim)
        assert self.shape.finaldim * 2**wide_log2 == data_unpacked.width
        padded_address = rtl.concat(rtl.Signal.from_bits([rtl.BitState.S0] * wide_log2), addr)

        with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
            rtl.MEMWR(m,
                self.name,
                clk=self.d.rtl_clk,
                **rtl.wrap_in_timeportal2(m,
                    en=(en_wide, False),
                    addr=(padded_address, False),
                    data=(data_unpacked, False),
                    bank_a_domain=self.name,
                    bank_a_constraint="fixed",
                )
            )
            ei_width = self.d.execid_width
            shadow_data = rtl.concat(rtl.HIGH, auxdata[:ei_width])
            rtl.MEMWR(m,
                self.shadow_name,
                clk=self.d.rtl_clk,
                **rtl.wrap_in_timeportal2(m,
                    en=(rtl.repeat(en, shadow_data.width * 2**wide_log2), False),
                    addr=(padded_address, False),
                    data=(rtl.repeat(shadow_data, 2**wide_log2), False),
                    bank_a_domain=self.name,
                    bank_a_constraint="fixed",
                )
            )

    @property
    def auxdatalen(self):
        return self.d.execid_width


class AddressableImplDescriptor:
    def __init__(self, backings):
        self.backings = backings

    @classmethod
    def max(self, args, *rest):
        backings = []
        for arg in args:
            for back in arg.backings:
                if back not in backings:
                    backings.append(back)
        return AddressableImplDescriptor(backings)

    @property
    def datalen(self):
        return len(self.backings) + self.auxdatalen

    @property
    def auxdatalen(self):
        # TODO: empty variable with no backing
        return max(b.auxdatalen for b in self.backings)

    def read(self, bi, en, addr, data, impldata, transparent=False):
        m = bi.f.d.rtl_module
        cases = []
        backing_sel = impldata[impldata.width - len(self.backings):]
        for back, enbit in zip(self.backings, backing_sel.bits):
            bdata = SignalValue(m.add_wire("$mem_out", data.shape.bitlen), data.shape)
            back.read(bi, rtl.AND(m, en, rtl.Signal.from_bits([enbit])), addr, bdata,
                      impldata[:back.auxdatalen], transparent=transparent)
            cases.append((rtl.Signal.from_bits([enbit]), bdata.extract_underlying_signal()))
        m.connect(data.extract_underlying_signal(),
                  rtl.build_mux(m, cases, width=data.shape.bitlen)[0])

    def write(self, bi, en, addr, data, impldata):
        m = bi.f.d.rtl_module
        backing_sel = impldata[impldata.width - len(self.backings):]
        for back, enbit in zip(self.backings, backing_sel.bits):
            back.write(bi, rtl.AND(m, en, rtl.Signal.from_bits([enbit])),
                       addr, data, impldata[:back.auxdatalen])


class ValueAddressable(Value):
    @graph_property
    def desc(self):
        raise NotImplementedError()


class SubbedBacking(AddressableVarBacking):
    def __init__(self, updesc, subbedbits):
        self.updesc = updesc
        self.subbedbits = subbedbits

    def read(self, bi, en, addr, data, auxdata, transparent=False):
        addr_pre = auxdata[auxdata.width - self.subbedbits:]
        self.updesc.read(bi, en, rtl.concat(addr, addr_pre),
                         data, auxdata[:auxdata.width - self.subbedbits],
                         transparent)

    def write(self, bi, en, addr, data, auxdata):
        addr_pre = auxdata[auxdata.width - self.subbedbits:]
        self.updesc.write(bi, en, rtl.concat(addr, addr_pre),
                          data, auxdata[:auxdata.width - self.subbedbits])

    @property
    def auxdatalen(self):
        return self.subbedbits + self.updesc.datalen


class VarValueAddressable(ValueAddressable):
    def __init__(self, var, bi, addr=[]):
        self.addr = addr
        self.var = var
        self.bi = bi
        self.shape = self.var.shape.drop_dims(len(addr))
        self.subbedbits = sum((s - 1).bit_length() for s in var.shape.dims[:len(addr)])

    @property
    def wire(self):
        if self.subbedbits == 0:
            return self.var.get_impldata_wire(self.bi)
        else:
            addrbits = VarImplAddressable.format_address(self.addr, self.var.shape.dims)
            assert addrbits.width == self.subbedbits
            return rtl.concat(self.var.get_impldata_wire(self.bi), addrbits, rtl.HIGH)

    @graph_property
    @cache
    def desc(self):
        if self.subbedbits == 0:
            return successor_maximum([
                (self.var.desc, self.var)
            ], AddressableImplDescriptor.max)
        else:
            return AddressableImplDescriptor([
                SubbedBacking(self.var.desc(), self.subbedbits)
            ])

    def extract_underlying_signal(self):
        sig = self.var.f.design.rtl_module.add_wire("$addressable_ret", self.shape.bitlen)
        m = self.var.f.design.rtl_module
        transparent = bool(hint_pre("transp"))
        def build():
            self.var.desc().read(self.bi, self.bi.en,
                VarImplAddressable.format_address(self.addr, self.var.shape.dims[:len(self.addr)]),
                SignalValue(sig, self.shape), self.var.get_impldata_wire(self.bi),
                transparent=transparent)
        self.var._delegated.append(build)
        return sig

    def sub(self, m, *addr):
        return VarValueAddressable(self.var, self.bi, self.addr + addr)


class VarImplAddressable(BaseVarImpl):
    def __init__(self, *args, backing=True):
        super().__init__(*args)

        if backing:
            self.backing = MemoryVarBacking(self.f,
                                            self.varname, self.shape)
        else:
            self.backing = None

        self._impldata_wires = dict()
        self._assigns = []
        self._evals = []
        self._injected = []
        self._delegated = []
        self.built = False
        self.has_desc = False

    @classmethod
    def format_address(self, addr, dims):
        return rtl.concat(*reversed([rtl.adjust_width(sig.extract_signal(), (s - 1).bit_length())
                                     for sig, s in zip(addr, dims)]))

    def inject_arg(self, bi, en, val):
        assert not self.built
        assert not self.has_desc
        if not isinstance(val, ValueAddressable):
            raise BadInput("function requires an addressable argument")
        self._injected.append((bi, en, val))

    def assign(self, bi, val, *addr):
        self._assigns.append((bi, val.cast(self.shape.drop_dims(len(addr))), addr))

    def eval(self, bi, *addr):
        return VarValueAddressable(self, bi, addr)

    @graph_property
    @cache
    def desc(self):
        self.has_desc = True
        if not self.backing:
            return successor_maximum([(val.desc, val) for _, _, val in self._injected],
                                      AddressableImplDescriptor.max)
        else:
            return AddressableImplDescriptor([self.backing])

    def get_impldata_wire(self, bi):
        desc = self.desc()
        ei_width = self.f.design.execid_width
        m = self.f.design.rtl_module

        if self.backing:
            assert desc.datalen == ei_width + 1
            return rtl.concat(self.f.frame_execid(bi), rtl.HIGH)

        retwire_execid = self.m.add_wire(f"${self.varname}_impldata_execid", ei_width)
        retwire = self.m.add_wire(f"${self.varname}_impldata", desc.datalen - ei_width)

        self.m.add_cell("\\VAR_GET",
            ("\\Q", retwire_execid),
            WIDTH=ei_width,
            AT_NODE=bi.id,
            NAMESPACE=self.f.namespace,
            NAME=f"${self.varname}_impldata_execid",
        )
        self.m.add_cell("\\VAR_GET",
            ("\\Q", retwire),
            WIDTH=desc.datalen - ei_width,
            AT_NODE=bi.id,
            NAMESPACE=self.f.namespace,
            NAME=f"${self.varname}_impldata",
        )

        return rtl.concat(retwire_execid, retwire)

    @classmethod
    def splice(self, design, desc1, data1, desc2):
        selbits1 = data1[data1.width - len(desc1.backings):]
        selbits2 = []
        for back in desc2.backings:
            if back not in desc1.backings:
                selbits2.append(rtl.BitState.S0)
            else:
                idx = desc1.backings.index(back)
                selbits2.append(selbits1.bits[idx])

        auxdata2 = rtl.concat(data1[:min(desc1.auxdatalen, desc2.auxdatalen)],
                              rtl.repeat(rtl.UNDEF, max(0, desc2.auxdatalen - desc1.auxdatalen)))
        return rtl.concat(auxdata2, rtl.Signal.from_bits(selbits2))

    def build(self):
        desc = self.desc()
        ei_width = self.f.design.execid_width
        m = self.f.design.rtl_module

        if self.backing is None:
            for bi, en, val in self._injected:
                spliced = self.splice(m, val.desc(), val.wire, desc)

                # Special-case the bottom execid bits of the wire for the synthesis
                # passes to have easy time optimizing those out
                with rtl.SynthAttrContext(keep=True):
                    self.m.add_cell("\\VAR_SET",
                        ("\\D", spliced[:ei_width]),
                        ("\\EN", en),
                        WIDTH=ei_width,
                        AT_NODE=bi.id,
                        NAMESPACE=self.f.namespace,
                        NAME=f"${self.varname}_impldata_execid",
                    )

                with rtl.SynthAttrContext(keep=True):
                    self.m.add_cell("\\VAR_SET",
                        ("\\D", spliced[ei_width:]),
                        ("\\EN", en),
                        WIDTH=self.desc().datalen - ei_width,
                        AT_NODE=bi.id,
                        NAMESPACE=self.f.namespace,
                        NAME=f"${self.varname}_impldata",
                    )
        else:
            pass

        for bi, val, addr in self._assigns:
            desc.write(bi, bi.en, self.format_address(addr, self.shape.dims[:len(addr)]),
                       val, self.get_impldata_wire(bi))

        for d in self._delegated:
            d()

        self.built = True


class VarValueMutAddressable(Value):
    def __init__(self, var, bi, addr=[]):
        assert isinstance(var, VarImplMutAddressable)
        self.var = var
        self.bi = bi
        self.shape = self.var.shape.drop_dims(len(addr))
        self.addr = addr

    def extract_underlying_signal(self):
        return self.var._read(self.bi, *self.addr)

    def sub(self, m, *addr):
        return VarValueMutAddressable(self.var, self.bi, self.addr + addr)


class VarImplMutAddressable(BaseVarImpl):
    def __init__(self, *args, backing=True):
        super().__init__(*args)
        self.d = self.f.d
        self.addrdims = self.shape.ndims - 1

        # TODO
        self.name = f"\\mem_{self.varname}"
        self.shadow_name = f"\\shadow_{self.varname}"

        m = self.d.rtl_module
        shape = self.shape

        attrs = {}
        if hint("ramstyle"):
            attrs["ramstyle"] = hint("ramstyle")
        with rtl.SynthAttrContext(**attrs):
            m.add_memory(self.name, product(*shape.dims[:-1]), shape.finaldim)

        m.add_memory(self.shadow_name + "_write_valid", product(*shape.dims[:-1]), 1)
        rtl.MEMINIT(m, self.shadow_name + "_write_valid",
            addr=rtl.Signal.from_bits([]),
            data=rtl.repeat(rtl.LOW, product(*shape.dims[:-1])),
            width=1,
        )
        m.add_memory(self.shadow_name + "_read_valid", product(*shape.dims[:-1]), 1)
        rtl.MEMINIT(m, self.shadow_name + "_read_valid",
            addr=rtl.Signal.from_bits([]),
            data=rtl.repeat(rtl.LOW, product(*shape.dims[:-1])),
            width=1,
        )
        m.add_memory(self.shadow_name + "_write", product(*shape.dims[:-1]), self.d.execid_width)
        m.add_memory(self.shadow_name + "_read", product(*shape.dims[:-1]), self.d.execid_width)

        self.read_triggers = []
        self.write_triggers = []

    @classmethod
    def format_address(self, addr, dims):
        return rtl.concat(*reversed([rtl.adjust_width(sig.extract_signal(), (s - 1).bit_length())
                                     for sig, s in zip(addr, dims)]))

    def inject_arg(self, bi, en, val):
        raise RuntimeError("mutable variables cannot be arguments")

    def _memwr(self, name, en, addr, data, **kwargs):
        m = self.d.rtl_module
        addr_formatted = self.format_address(addr, self.shape.dims) \
                         if not isinstance(addr, rtl.Signal) else addr
        rtl.MEMWR(m, name,
            clk=self.d.rtl_clk,
            **rtl.wrap_in_timeportal2(m,
                en=(en, False),
                addr=(addr_formatted, False),
                data=(data, False),
                bank_a_domain=name,
                bank_a_constraint="fixed",
            ),
            **kwargs
        )

    def _memrd(self, name, en, addr, data_width, transp=False):
        m = self.d.rtl_module
        ret = m.add_wire("$memrd_data", data_width)
        addr_formatted = self.format_address(addr, self.shape.dims) \
                         if not isinstance(addr, rtl.Signal) else addr
        rtl.MEMRD(m, name,
            clk=self.d.rtl_clk,
            **rtl.wrap_in_timeportal2(m,
                en=(en, False),
                addr=(addr_formatted, False),
                data=(rtl.SEER(m, ret, 1, backwards=True), True),
                bank_a_domain=name,
                bank_a_constraint="fixed",
            ),
            transparent=transp,
        )
        return ret

    def _read(self, bi, *addr):
        assert len(addr) <= self.addrdims
        if len(addr) != self.addrdims:
            return rtl.concat(*(
                self._read(bi, addr + (j,))
                for j in range(self.shape.dims[len(addr)])
            ))

        transp = bool(hint_pre("transp"))
        m = self.d.rtl_module
        if transp:
            self.read_triggers.append((rtl.SEER(m, bi.en, -1),
                rtl.SEER(m, self.format_address(addr, self.shape.dims), -1),
                rtl.SEER(m, bi.execid, -1)))
        else:
            self.read_triggers.append((bi.en, self.format_address(addr, self.shape.dims), bi.execid))

        # TODO: consider frame switches
        with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
            curr_execid = bi.execid
            last_write = self._memrd(self.shadow_name + "_write", bi.en, addr, self.d.execid_width, transp=transp)
            last_write_valid = self._memrd(self.shadow_name + "_write_valid", bi.en, addr, 1, transp=transp)
            assert_en = rtl.AND(m, bi.en, last_write_valid) # if there was any last write
            with rtl.SynthAttrContext(execid_assert=True):
                rtl.ASSERT(m, assert_en,
                    execid.GE(m, curr_execid, last_write, assert_en) # this read must be ordered after the write
                )
            return self._memrd(self.name, bi.en, addr, self.shape.finaldim, transp=transp)

    @classmethod
    def _unpack(self, shape, packed_signal, padbit=rtl.BitState.Sx):
        padword = rtl.Signal.from_bits([padbit] * shape.finaldim)
        return rtl.concat(*[
            (packed_signal.slot(shape.finaldim, encode_multiindex(j, shape.dims[:-1]))
                if all([a < b for a, b in zip(j, shape.dims)]) else padword)
            for j in mrange(*[2 ** ((s-1).bit_length()) \
                              for s in shape.dims[:-1]])
        ])

    def assign(self, bi, val, *addr):
        m = self.d.rtl_module
        val = val.cast(self.shape.drop_dims(len(addr)))

        if hint_pre("meminit"):
            rtl.MEMINIT(m, self.name,
                addr=self.format_address(addr, self.shape.dims),
                data=self._unpack(val.shape, val.extract_underlying_signal()),
                width=self.shape.finaldim,
            )
            return

        assert len(addr) <= self.addrdims
        if len(addr) != self.addrdims:
            for j in range(self.shape.dims[len(addr)]):
                self.assign(bi, val.sub(m, SignalValue.from_const(j)),
                            *(addr + (SignalValue.from_const(j),)))
            return

        self.write_triggers.append((bi.en, self.format_address(addr, self.shape.dims)))

        with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
            self._memwr(self.name, bi.en, addr, val.extract_underlying_signal())

            curr_execid = bi.execid
            self._memwr(self.shadow_name + "_write", bi.en, addr, bi.execid)
            self._memwr(self.shadow_name + "_write_valid", bi.en, addr, rtl.HIGH)

            last_read = self._memrd(self.shadow_name + "_read", bi.en, addr, self.d.execid_width)
            last_read_valid = self._memrd(self.shadow_name + "_read_valid", bi.en, addr, 1)

            assert_en = rtl.AND(m, bi.en, last_read_valid) # if there was a read since last write
            with rtl.SynthAttrContext(execid_assert=True,
                                      guards_memoryvar=self.name):
                rtl.ASSERT(m, assert_en, rtl.OR(m,
                    execid.GE(m, curr_execid, last_read, assert_en), # the read must be ordered before this write
                ))

    def eval(self, bi, *addr):
        return VarValueMutAddressable(self, bi, addr)

    def build(self):
        # writes cannot be to the same address in the same cycle
        m = self.d.rtl_module
        for i in range(len(self.write_triggers)):
            for j in range(i):
                en0, addr0 = self.write_triggers[i]
                en1, addr1 = self.write_triggers[j]
                rtl.ASSERT(m,
                    rtl.AND(m,
                        en0, en1,
                        rtl.EQ(m, addr0, addr1)
                    ),
                    rtl.LOW
                )

        for i in range(len(self.read_triggers)):
            en0, addr0, execid0 = self.read_triggers[i]

            old_read = rtl.SEER(m,
                self._memrd(self.shadow_name + "_read", rtl.SEER(m, en0, 2),
                            rtl.SEER(m, addr0, 2), self.d.execid_width, transp=True), -2)
            old_read_valid = rtl.SEER(m,
                self._memrd(self.shadow_name + "_read_valid", rtl.SEER(m, en0, 2),
                            rtl.SEER(m, addr0, 2), 1, transp=True), -2)

            # if there was a write the cycle before, this is the first cycle
            # the new value can be read and we are starting with a clean slate
            cancel_old = rtl.OR(m, *[
                rtl.AND(m, rtl.SEER(m, en, -1), rtl.EQ(m, addr0, rtl.SEER(m, addr, -1)))
                for en, addr in self.write_triggers
            ])

            next_read, next_read_valid = execid.MAX(m, [
                (rtl.AND(m, en0, old_read_valid, rtl.NOT(m, cancel_old)), old_read)
            ] + [
                (rtl.AND(m, en, rtl.EQ(m, addr0, addr)), execid)
                for en, addr, execid in self.read_triggers[:i + 1]
            ], y_width=self.d.execid_width)

            write_en0 = rtl.AND(m,
                en0,
                *[
                    rtl.NOT(m, rtl.AND(m, en, rtl.EQ(m, addr0, addr)))
                    for en, addr, execid in self.read_triggers[i + 1:]
                ]
            )

            self._memwr(self.shadow_name + "_read", rtl.SEER(m, write_en0, 1), rtl.SEER(m, addr0, 1), rtl.SEER(m, next_read, 1))
            self._memwr(self.shadow_name + "_read_valid", rtl.SEER(m, write_en0, 1), rtl.SEER(m, addr0, 1), rtl.SEER(m, next_read_valid, 1))
