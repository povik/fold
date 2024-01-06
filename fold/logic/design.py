# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from functools import cache
import io

from ..ast import *
from ..eval import *
from ..utils import *

from .eval import CombinatorialEvaluator, BASIC_OPS
from .shape import *
from .vars import *
from .graph import *
from .ops import SPECIAL_OPS

from . import rtl, execid


class BlockImpl:
    def __init__(self, frame, label="", markers=None):
        self.markers = markers or Tuple.curr_markers
        self.frame = frame
        self.label = label if label != "" else f"${frame.d.make_up_id()}"

        self.en = frame.d.rtl_module.add_wire(f"$en${label}", 1,
                                              **{'seer.background_value': rtl.Const.from_const(0, 1)})
        self.en_sources = []
        self.execid = frame.d.rtl_module.add_wire(f"$execid${label}", frame.d.execid_width)
        self.execid_sources = []

    @property
    def f(self):
        return self.frame

    @property
    def src(self):
    	return markers_str(self.markers)

    @property
    def full_label(self):
        prefix = self.f.namespace.replace(" ", ".")
        return prefix + "." + self.label.replace(" ", "")

    def __repr__(self):
        return "<blockimpl %s>" % self.full_label

    def build(self):
        m = self.frame.design.rtl_module
        ei_width = self.frame.design.execid_width
        with rtl.SynthAttrContext(src=self.src):
            m.connect(self.en, rtl.OR(m, *self.en_sources))
            m.connect(self.execid, rtl.PMUX(m, self.en_sources,
                                            self.execid_sources, width=ei_width))

        with rtl.SynthAttrContext(fold_debug=True,
                                  for_blockimpl=self.full_label,
                                  src=self.src):
            m.add_cell_keep(
                "\\MUTEX_ASSERT",
                ("\\CLK", self.frame.d.rtl_clk),
                ("\\A", rtl.concat(*self.en_sources)),
                WIDTH=len(self.en_sources),
                MESSAGE=f"activation of {self!s}",
            )

    @classmethod
    def en(self, bimpl):
        if bimpl is None:
            return rtl.LOW
        return bimpl.en

    @property
    def id(self):
        '''
        ID for this node to use in the emitted circuit
        '''
        return escape_id(self.full_label)

class ImmutLink(Edge):
    EP1_PROPS = ('hot', '_xfer')
    EP2_PROPS = ('hot_back', '_xfer_back')

    def __init__(self, ep1, ep2, hot, xfer, hot_back, xfer_back):
        super().__init__(ep1, ep2)

        assert ep1 is not ep2

        self.hot, self.hot_back = hot, hot_back
        self._xfer, self._xfer_back = xfer, xfer_back
        self.tuples = Tuple.curr_markers
        self.threshold = None

    def __str__(self):
        return f"immutlink({self.ep1} -> {self.ep2})"

    def __repr__(self):
        return f"<{self!s}>"

    def xfer(self, *args, **kwargs):
        with rtl.SynthAttrContext(implements_link=str(self)):
            return self._xfer(*args, **kwargs)

    def xfer_back(self, *args, **kwargs):
        with rtl.SynthAttrContext(implements_link=str(self.rev)):
            return self._xfer_back(*args, **kwargs)

    def applies_in_frame(self, frame):
        return self.threshold is None or (frame.parent_frame and
                                          self.threshold in frame.parent_frame.stack)

    @property
    def internal_labels(self):
        return ([] if self._xfer == Id() else [f"{self._xfer}"])


class Frame:
    def __init__(self, design, parent, framename=""):
        if framename == "":
            framename = f"${design.make_up_id()}"
        self.framename = framename
        self.design = design
        self.parent_frame = parent
        if parent:
            parent.children_frames.add(self)
        self.children_frames = set()

        self.labels = {}

        self.vars = dict()
        self.blockimpls = set()

        if parent is not None:
            self.immutlinks = parent.immutlinks.add_subgraph()
        else:
            self.immutlinks = Graph()
        self.mutlinks = DirectedGraph()

        self.fg_substitutions = {}
        self.is_function = False

    def frame_execid(self, bi):
        m = self.design.rtl_module

        retwire = m.add_wire(f"$execid", self.design.execid_width)
        m.add_cell("\\VAR_GET",
            ("\\Q", retwire),
            WIDTH=retwire.width,
            AT_NODE=bi.id,
            NAMESPACE=self.namespace,
            NAME=f"$execid",
        )
        return retwire

    @property
    def children_recursive(self):
        return self.children_frames.union(set([
            grandchild for child in self.children_frames
            for grandchild in child.children_recursive
        ]))

    @classmethod
    def common_parent(self, a, b):
        return [f for f in a.stack if f in b.stack][0]

    @property
    def d(self):
        return self.design

    @property
    def stack(self):
        f = self
        while f is not None:
            yield f
            f = f.parent_frame

    def register_bimpl(self, bimpl):
        self.blockimpls.add(bimpl)
        self.immutlinks.add_node(bimpl)

        for f in self.stack:
            f.mutlinks.add_node(bimpl)

    def impl_var(self, vardecl, injected=False):
        with vardecl as (varname, shapenode):
            shape, mutable = self.d.eval_shape(shapenode)
            if not mutable:
                if hint("global"):
                    raise BadInput("immutable variables cannot be global")
                if hint("mem"):
                    if injected:
                        raise BadInput("arguments and return values cannot have the '{:h}' hint", "mem")
                    varimpl = VarImplAddressable(self, varname, shape)
                elif hint("addr"):
                    if not injected:
                        raise BadInput("the '{:h}' hint is reserved for arguments and return values", "addr")
                    varimpl = VarImplAddressable(self, varname, shape, backing=False)
                else:
                    varimpl = VarImplSignal(self, varname, shape)
                self.vars[varname] = varimpl
            else:
                if hint("addr"):
                    # TODO: can addr be here on mut?
                    raise BadInput("addressable variables cannot be mutable")
                if hint("mem"):
                    self.vars[varname] = \
                        VarImplMutAddressable(self, varname, shape)
                elif hint("global"):
                    self.vars[varname] = \
                        VarImplMutableGlobal(self, varname, shape)
                else:
                    self.vars[varname] = \
                        VarImplMutable(self, varname, shape)

    def find_var(self, varname):
        for f in self.stack:
            if varname in f.vars:
                return f, f.vars[varname]

        raise KeyError(varname)

    @property
    def depth(self):
        return len(list(self.stack))

    def build(self):
        for var in self.vars.values():
            var.build()

    @property
    def namespace(self):
        nodes = reversed([escape_id(f_.framename) for f_ in self.stack][:-1])
        return ' '.join(nodes)


CALL_COUNTER = 3
def impl_callsite(bseq, expr, args, wait=False):
    global CALL_COUNTER
    CALL_COUNTER += 1

    m = bseq.d.rtl_module

    op_bseq = bseq.d.request_func_blockseq(expr.opname)
    op_ast = bseq.d.get_function_ast(expr.opname)

    departing = bseq.curr
    bseq.rewind()
    arriving = bseq.curr

    to_entry = bseq.immutlink(departing, op_bseq.entry,
                              inject_execid=execid.DESCEND(m, departing.execid, departing.en))
    bseq.mutlink(departing, op_bseq.entry, to_entry, hot=departing.en,
                 imprint_pop=CALL_COUNTER)

    m = bseq.frame.design.rtl_module

    if wait:
        bseq_xform = BlockSeqTransform(op_bseq)
        hot = bseq_xform(departing.en)
        from_exit = bseq.immutlink(op_bseq.exit, arriving, hot=hot,
                                   inject_execid=execid.INC(m, bseq_xform(departing.execid), arriving.en),
                                   do_not_set_frameid=True)
        bseq.mutlink(op_bseq.exit, arriving, from_exit, hot=rtl.AND(m, op_bseq.exit.en, hot),
                     imprint_push=CALL_COUNTER)

        phantom_stalk = bseq.immutlink_phantom(
            departing, arriving,
            transform=bseq_xform,
            threshold=Frame.common_parent(bseq.frame, op_bseq.frame)
        )

        sub = to_entry * op_bseq.runthrough * from_exit
        for f in Frame.common_parent(bseq.frame, op_bseq.frame).stack:
            f.fg_substitutions[phantom_stalk] = sub
            f.fg_substitutions[phantom_stalk.inv] = sub.inv

        for f in bseq.frame.stack:
            if f not in op_bseq.frame.stack:
                f.mutlinks.link(MutLink(departing, arriving, phantom_stalk, rtl.HIGH))
    else:
        to_arriving = bseq.immutlink(departing, arriving)
        bseq.mutlink(op_bseq.exit, arriving, op_bseq.runthrough.inv * to_entry.inv *
                                             to_arriving, hot=departing.en, imprint_push=CALL_COUNTER)
        for f in bseq.frame.stack:
            if f not in op_bseq.frame.stack:
                f.mutlinks.link(MutLink(departing, arriving,
                                        to_arriving, rtl.HIGH))

    argsdecl = op_ast[2]
    if len(expr.args) != len(argsdecl):
        raise BadInput("bad number of arguments in call of function {:h}, given {} arguments expected {}",
                       expr.opname, len(expr.args), len(argsdecl))

    _1, _2, argsdecl, retsdecl, body = op_ast
    for argdecl, arg in zip(argsdecl, args):
        with argdecl as (argname, shapenode):
            shape, mutable = bseq.d.eval_shape(shapenode)
            var = op_bseq.frame.vars[argname]
        var.inject_arg(op_bseq.entry, departing.en, arg)

    return op_bseq, op_ast


class BlockSeqEval(CombinatorialEvaluator):
    def __init__(self, bseq):
        self.bseq = bseq
        self.d = bseq.frame.design
        super().__init__(self.d.rtl_module)

    def on_Const(self, expr):
        if type(expr.val) is str:
            return SignalValue(
                rtl.encode_string(expr.val),
                Shape(len(expr.val.encode("ascii")), 8, signed=False),
            )
        elif type(expr.val) is int:
            return SignalValue.from_const(expr.val)
        else:
            raise ValueError(expr.val)

    def on_Var(self, expr):
        if expr.varname == "undef":
            return SignalValue(rtl.Signal.from_bits([rtl.BitState.Sx]), Shape(1, signed=True))

        if expr.varname in self.d._consts:
            val = self.d._consts[expr.varname]
            width = max(val.bit_length(), 1) \
                    + (val < 0)
            return SignalValue(
                rtl.Signal.from_const(val, width),
                Shape(width, signed=val < 0)
            )
        try:
            f, var = self.bseq.frame.find_var(expr.varname)
        except KeyError:
            raise BadInput("no such variable: {:h}", expr.varname)

        with expr:
            return var.eval(self.bseq.curr)

    def on_Op(self, expr):
        with expr:
            with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
                opname = expr.opname

                if opname in SPECIAL_OPS:
                    return SPECIAL_OPS[opname](self.bseq, expr)

                # TODO: fix duplicate evaluation of op arguments
                val = super().on_Op(expr)
                if val is not None:
                    return val

                args = [self(node) for node in expr.args]

                if opname == ":":
                    return (":", *args)

                if opname == "..":
                    return ("..", *args)

                if opname == "[":
                    m = self.bseq.f.design.rtl_module
                    return args[0].sub(m, *args[1:])

                op_bseq, ast = impl_callsite(self.bseq, expr, args, wait=False)

                retsdecl = ast[3]
                if len(retsdecl) != 1:
                    raise BadInput("single return value function required in expression context ({:h} returns {:h} values)",
                                   opname, len(retsdecl))

                return op_bseq.frame.vars[retsdecl[0][0]].eval(op_bseq.entry)


class Transform:
    @property
    def has_inv(self):
        return False

    @property
    def inv(self):
        raise NotImplementedError


class Id(Transform):
    @property
    def has_inv(self):
        return True

    @property
    def inv(self):
        return self

    def __call__(self, sig, zeroed=False):
        return sig

    def __eq__(self, other):
        if type(other) is self.__class__:
            return True


class FixedOffset(Transform):
    def __init__(self, m, offset):
        self.m = m
        self.offset = offset

    @property
    def has_inv(self):
        return True

    @property
    def inv(self):
        return FixedOffset(self.m, -self.offset)

    def __call__(self, sig, zeroed=False):
        return rtl.SEER(self.m, sig, self.offset)

    def __str__(self):
        return f"<FONT color='brown'>fixed({self.offset})</FONT>"


class BackedgePortal(Transform):
    def __init__(self, m, cell=None):
        self.m = m
        self.cell = cell or m.add_cell("\\BACKEDGE",
            ("\\A", rtl.Signal.from_bits([])),
            ("\\AY", rtl.Signal.from_bits([])),
            ("\\B", rtl.Signal.from_bits([])),
            ("\\BY", rtl.Signal.from_bits([])),
            A_WIDTH=0,
            B_WIDTH=0,
        )
        self._is_inv = False

    @property
    def has_inv(self):
        return True

    @property
    def inv(self):
        ret = BackedgePortal(self.m, self.cell)
        ret._is_inv = not self._is_inv
        return ret

    def __call__(self, sig, zeroed=False):
        if isinstance(sig, rtl.Const):
            return sig

        retsig = self.m.add_wire("$backedge_out", sig.width)
        if not self._is_inv:
            self.cell.append_on_port("\\A", rtl.SEER(self.m, sig, -1))
            self.cell.append_on_port("\\AY", retsig)
            self.cell.set_param("B_WIDTH", self.cell.get_port("\\A").width)
        else:
            self.cell.append_on_port("\\B", rtl.SEER(self.m, sig, 1))
            self.cell.append_on_port("\\BY", retsig)
            self.cell.set_param("B_WIDTH", self.cell.get_port("\\B").width)
        return retsig

    def __str__(self):
        return f"<FONT color='brown'>backedge{'^{-1}' if self._is_inv else ''}</FONT>"


class BlockSeqTransform(Transform):
    def __init__(self, bseq, inversed=False):
        self.bseq = bseq
        self.inversed = inversed
        self._canon_inv = None

    @property
    def has_inv(self):
        return True

    @property
    def inv(self):
        if self._canon_inv is None:
            inv = BlockSeqTransform(self.bseq, not self.inversed)
            inv._canon_inv = self
            self._canon_inv = inv
        return self._canon_inv

    @cache
    def __call__(self, sig, zeroed=False):
        a, b = self.bseq.entry, self.bseq.exit
        if self.inversed:
            a, b = b, a
        m = self.bseq.d.rtl_module

        imported = m.add_wire("$imported", sig.width)
        m.add_cell("\\IMPORT",
            ("\\D", sig),
            ("\\Q", imported),
            WIDTH=sig.width,
            ZEROED=zeroed,
            NAMESPACE=self.bseq.frame.namespace,
            STALK="*",
            FROM_NODE=a.id, TO_NODE=b.id,
        )
        return imported

    def __str__(self):
        return f"<FONT color='purple'>bseq</FONT>"


class BlockSeq:
    def __init__(self, d, parent_frame=None, framename="", is_function=False):
        self.d = d
        self.f = self.frame = Frame(d, parent=parent_frame,
                                    framename=framename)
        self.f.is_function = is_function
        self.rewind("%entry")
        self.entry = self.curr
        self._exit = None
        self.finalized = False
        self.eval = BlockSeqEval(self)

    def rewind_to(self, bimpl):
        self.curr = bimpl

    def rewind(self, *args, **kwargs):
        self.rewind_to(self.create_node(*args, **kwargs))
        return self.curr

    def create_node(self, *args, **kwargs):
        bimpl = BlockImpl(self.frame, *args, **kwargs)
        self.frame.register_bimpl(bimpl)
        self.d.blockimpls.add(bimpl)
        return bimpl

    @property
    def exit(self):
        assert self.finalized
        return self._exit

    def finalize(self):
        self.finalized = True
        self._exit = self.curr
        if self.exit is not None:
            self.exit.label = "%exit"

    @property
    def runthrough_simple(self):
        assert self.finalized
        p = self.exit
        voffset = 0
        if self.exit is None:
            return False
        while p != self.entry:
            hit = False
            for edge in self.f.immutlinks.walk_eps(p):
                if edge.hot == rtl.HIGH and edge.hot_back == rtl.LOW \
                        and (edge._xfer == Id() or isinstance(edge._xfer, FixedOffset)):
                    if hit:
                        return False
                    hit = True
                    p = edge.tail
                    voffset += 0 if edge._xfer == Id() else edge._xfer.offset
                elif edge.hot == rtl.LOW:
                    continue
                else:
                    return False
            if not hit:
                return False
        # We have found `self.exit` is reached with a simple path from `self.entry`,
        # now check the composed transformation is an empty one
        return voffset == 0

    @property
    def runthrough(self):
        assert self.finalized

        if True and self.runthrough_simple:
            crumbs = Identity()
            p = self.exit
            while p != self.entry:
                hit = False
                for edge in self.f.immutlinks.walk_eps(p):
                    if edge.hot == rtl.HIGH and edge.hot_back == rtl.LOW \
                            and (edge._xfer == Id() or isinstance(edge._xfer, FixedOffset)):
                        assert not hit
                        hit = True
                        p = edge.tail
                        # TODO: do not access _link_fg_edges directly
                        crumbs *= self.f.immutlinks.root._link_fg_eles[edge].inv
                    elif edge.hot == rtl.LOW:
                        continue
                    else:
                        assert False
                assert hit
            return crumbs.inv

        return SmearedGroupele.of_subgraph(self.entry,
            self.exit, self.frame.immutlinks,
            mask=lambda e: e.applies_in_frame(self.frame))

    def immutlink(self, a, b, hot=None, transform=Id(), do_not_set_frameid=False, execid_fork=False,
                  inject_execid=None):
        if a is None or b is None:
            return Identity()

        if hot is None:
            hot = rtl.HIGH

        for f in self.frame.stack:
            if not (a in f.immutlinks.nodes and b in f.immutlinks.nodes):
                # We might be linking to a node outside current frame (because
                # of labels resolving to bimpls from parent frames). If that
                # happens, we need to skip some tip of the stack here.
                continue
            break

        m = self.d.rtl_module

        trigger = rtl.AND(m, a.en, hot)
        trigger_xformed = transform(trigger, zeroed=True)
        b.en_sources.append(trigger_xformed)

        if inject_execid is None:
            execid_wip = transform(a.execid)
            bumped_execid = False
            for f_ in a.frame.stack:
                if f_ not in b.frame.stack:
                    assert not f_.is_function # TODO: add execid support for jumping out of function bodies
                    execid_wip = execid.ASCEND(m, execid_wip, trigger_xformed)
                    bumped_execid = True
            if execid_fork:
                execid_wip = execid.FORK(m, execid_wip, trigger_xformed)
            for f_ in b.frame.stack:
                if f_ not in a.frame.stack:
                    assert not f_.is_function # TODO: add execid support for jumping into function bodies
                    if not do_not_set_frameid:
                        m.add_cell("\\VAR_SET",
                            ("\\D", execid_wip),
                            ("\\EN", trigger_xformed),
                            WIDTH=execid_wip.width,
                            AT_NODE=b.id,
                            NAMESPACE=f_.namespace,
                            NAME=f"$execid",
                        )
                    execid_wip = execid.DESCEND(m, execid_wip, trigger_xformed)
                    bumped_execid = True
            if not bumped_execid:
                execid_wip = execid.INC(m, execid_wip, trigger_xformed)
            b.execid_sources.append(execid_wip)
        else:
            b.execid_sources.append(inject_execid)

        link = ImmutLink(
            a, b, hot, transform, rtl.LOW, transform.inv
        )
        return f.immutlinks.link(link)

    def immutlink_phantom(self, a, b, hot=None, transform=Id(), threshold=None):
        assert threshold is not None
        if a is None or b is None:
            return Identity()
        if hot is None:
            hot = rtl.HIGH
        link = ImmutLink(
            a, b, hot, transform, rtl.LOW, transform.inv
        )
        link.threshold = threshold
        return self.frame.immutlinks.link(link, forceelem=True)

    def mutlink(self, a, b, stalk, hot, fixup_imprints=False, **kwargs):
        if a is None or b is None:
            return
        assert hot is not None

        for f in self.frame.stack:
            if not (a in f.mutlinks.nodes and b in f.mutlinks.nodes):
                # We might be linking to a node outside current frame (because
                # of labels resolving to bimpls from parent frames). If that
                # happens, we need to skip some tip of the stack here.
                continue

            if False: # TODO
                if isinstance(stalk, GroupElement):
                    mapped_stalk = Identity()
                    for ele in GroupElement.free_cast(stalk):
                        mapped_stalk *= f.fg_substitutions.get(ele, ele)
                elif type(stalk) is SmearedGroupele:
                    mapped_base = Identity()
                    for ele in GroupElement.free_cast(stalk.base):
                        mapped_base *= f.fg_substitutions.get(ele, ele)

                    mapped_smear_all = []
                    for smear in stalk.smear:
                        mapped_smear = Identity()
                        for ele in GroupElement.free_cast(smear):
                            mapped_smear *= f.fg_substitutions.get(ele, ele)
                        if type(mapped_smear) is SmearedGroupele:
                            mapped_smear_all += mapped_smear.generators
                        else:
                            mapped_smear_all += [mapped_smear]
                    mapped_stalk = SmearedGroupele(mapped_base, mapped_smear_all)
                else:
                    raise NotImplementedError(stalk, type(stalk))
            else:
                mapped_stalk = stalk

            link = MutLink(a, b, mapped_stalk, hot, **kwargs)
            if fixup_imprints:
                fixup_goto_imprints(link)
            f.mutlinks.link(link)

    @classmethod
    def from_ast_body(self, d, ast, parent_frame=None,
                      injectvars=[], framename="", inject_labels={},
                      function=False):
        seq = BlockSeq(d, parent_frame, framename=framename,
                       is_function=function)
        for vardecl in injectvars:
            seq.frame.impl_var(vardecl, injected=True)
        seq.frame.labels.update(inject_labels)
        seq.impl_ast_body(ast)
        seq.finalize()
        return seq

    def lookup_label(self, label):
        frame = self.frame
        while frame is not None \
                and label not in frame.labels:
            frame = frame.parent_frame

        if frame is None:
            if label == "%break":
                raise BadInput("nowhere to break from")
            raise BadInput("no such label: {:h}", label)
        return frame.labels[label]

    def lookup_label_in_children(self, label):
        for frame in set([self.frame]).union(self.frame.children_recursive):
            if label in frame.labels:
                return frame.labels[label]

        raise BadInput("no such label: {:h}", label)

    def impl_assignment(self, lhs, val):
        addrs = []
        if isinstance(lhs, Op):
            with lhs:
                if lhs.opname != "[":
                    raise BadInput("bad operation on left-hand side of assignment")
                addrs = [self.eval(arg) for arg in lhs.args[1:]]
                lhs = lhs.args[0]
        with lhs:
            if not isinstance(lhs, Var):
                raise BadInput("bad expression on left-hand side of assignment")
        try:
            frame, var = self.frame.find_var(lhs.varname)
        except KeyError:
            raise BadInput("no such variable: {:h}", lhs.varname)
        var.assign(self.curr, val, *addrs)

    def impl_top_expr(self, lhslist, rhs, embedded=False):
        if type(rhs) is Op and rhs.opname == "delay":
            if len(lhslist) != 0 or len(rhs.args) != 1:
                raise BadInput("bad usage of {:h}", "delay")
            delay = self.d.eval_const(rhs.args[0])
            save_curr = self.curr
            self.rewind()
            to_curr = self.immutlink(save_curr, self.curr,
                                     transform=FixedOffset(self.d.rtl_module, -delay))
            self.mutlink(save_curr, self.curr, to_curr, hot=rtl.HIGH)
        elif type(rhs) is Op and (rhs.opname
                                not in (BASIC_OPS + list(SPECIAL_OPS.keys())) + ["["]):
            wait = bool(hint_pre("wait"))
            args = [self.eval(node) for node in rhs.args]

            entry = self.curr
            bseq, ast = impl_callsite(self, rhs, args, wait=wait)
            retsdecl = ast[3]

            curr_backup, self.curr = self.curr, entry
            for lhs, retdecl in zip(lhslist, retsdecl):
                with retdecl as (retname, _):
                    if not isinstance(bseq.f.vars[retname], VarImplAddressable):
                        continue
                    var = bseq.f.vars[retname]
                var.inject_arg(bseq.entry, entry.en, self.eval(lhs))
            self.curr = curr_backup

            if len(retsdecl) != len(lhslist):
                raise BadInput("assignment mismatch, function {:h} returns {} values, given {} sites",
                                rhs.opname, len(retsdecl), len(lhslist))

            for lhs, retdecl in zip(lhslist, retsdecl):
                if isinstance(lhs, Var) and lhs.varname == "_":
                    continue
                if isinstance(bseq.f.vars[retdecl[0]], VarImplAddressable):
                    continue
                self.impl_assignment(lhs,
                    bseq.frame.vars[retdecl[0]].eval(bseq.exit if wait else bseq.entry))
        else:
            if len(lhslist) > 1:
                raise BadInput("multiple value assignment of a single value expression result")
            res = self.eval(rhs)
            if len(lhslist) > 0:
                self.impl_assignment(lhslist[0], res)

    def eval_cond(self, ast):
        m = self.d.rtl_module
        seq = BlockSeq(self.d, self.f)
        stalk = self.immutlink(self.curr, seq.entry)
        self.mutlink(self.curr, seq.entry, stalk, hot=rtl.HIGH)
        cond_sig = rtl.REDUCE_BOOL(m, seq.eval(ast).extract_signal())
        seq.finalize()
        with rtl.SynthAttrContext(src=markers_str(ast.markers)):
            rtl.ASSERT(m,
                self.curr.en,
                rtl.NOT(m, rtl.EQX(m, cond_sig, rtl.UNDEF))
            )
        return cond_sig

    def impl_ast_body(self, ast):
        for stat in ast:
            if stat[0] == "label":
                with stat as (_, label):
                    self.rewind(label)
                self.frame.labels[label] = self.curr

        self.rewind_to(self.entry)

        m = self.d.rtl_module

        for stat in ast:
            if stat[0] == "label":
                with stat as (_, label):
                    old = self.curr
                    self.rewind_to(self.frame.labels[label])
                    to_new = self.immutlink(old, self.curr)
                    if old is not None:
                        self.mutlink(old, self.curr, to_new, hot=old.en)
            elif stat[0] == "=":
                with stat as (_, lhslist, rhslist):
                    if len(rhslist) == 1:
                        self.impl_top_expr(lhslist, rhslist[0])
                    elif len(lhslist) != len(rhslist):
                        raise BadInput("LHS/RHS mismatch")
                    else:
                        for lhs, rhs in zip(lhslist, rhslist):
                            self.impl_top_expr([lhs], rhs, embedded=True)
            elif isinstance(stat[0], list):
                with stat as (rhslist,):
                    for rhs in rhslist:
                        self.impl_top_expr([], rhs, embedded=(len(rhslist) != 1))
            elif stat[0] == "if":
                with stat as (_, cond, truebody, falsebody):
                    condsig = self.eval_cond(cond)

                    premise = self.curr
                    self.rewind(markers=(stat.markers[1], stat.markers[1]))
                    followup = self.curr

                    tseq = BlockSeq.from_ast_body(self.d, truebody, self.frame)
                    to_tseq_entry = self.immutlink(premise, tseq.entry, hot=condsig)
                    self.mutlink(premise, tseq.entry, to_tseq_entry, hot=rtl.HIGH)

                    condsig_not = rtl.NOT(m, condsig)

                    fseq = BlockSeq.from_ast_body(self.d, falsebody, self.frame)
                    to_fseq_entry = self.immutlink(premise, fseq.entry, hot=condsig_not)
                    self.mutlink(premise, fseq.entry, to_fseq_entry, hot=rtl.HIGH)

                    if True and (tseq.runthrough_simple and fseq.runthrough_simple):
                        to_followup = self.immutlink(premise, followup)
                        self.mutlink(tseq.exit, followup, tseq.runthrough.inv * to_tseq_entry.inv * to_followup,
                                     hot=BlockImpl.en(tseq.exit))
                        self.mutlink(fseq.exit, followup, fseq.runthrough.inv * to_fseq_entry.inv * to_followup,
                                     hot=BlockImpl.en(fseq.exit))
                    else:
                        if tseq.runthrough_simple:
                            to_followup = self.immutlink(premise, followup, hot=condsig)
                            self.mutlink(tseq.exit, followup, tseq.runthrough.inv * to_tseq_entry.inv * to_followup,
                                         hot=BlockImpl.en(tseq.exit))
                        else:
                            to_followup = self.immutlink(tseq.exit, followup)
                            self.mutlink(tseq.exit, followup, to_followup,
                                         hot=BlockImpl.en(tseq.exit))

                        if fseq.runthrough_simple:
                            to_followup = self.immutlink(premise, followup, hot=condsig_not)
                            self.mutlink(fseq.exit, followup, fseq.runthrough.inv * to_fseq_entry.inv * to_followup,
                                         hot=BlockImpl.en(fseq.exit))
                        else:
                            to_followup = self.immutlink(fseq.exit, followup)
                            self.mutlink(fseq.exit, followup, to_followup,
                                         hot=BlockImpl.en(fseq.exit))
            elif stat[0] == "var":
                with stat as (_, varlist):
                    for vardecl in varlist:
                        self.frame.impl_var(vardecl)
            elif stat[0] == "goto":
                with stat as (_, label):
                    t = Id()
                    if hint_pre("offset"):
                        t = FixedOffset(self.d.rtl_module, int(hint_pre("offset")))
                    target = self.lookup_label(label)

                    if self.curr is None:
                        continue

                    to_target = self.immutlink(self.curr, target, transform=t)
                    self.mutlink(self.curr, target, to_target, hot=t(self.curr.en), fixup_imprints=True)

                    self.curr = None
                    #self.rewind()
            elif stat[0] == "fork":
                with stat as (_, label):
                    t = Id()
                    if hint_pre("offset"):
                        t = FixedOffset(self.d.rtl_module, int(hint_pre("offset")))
                    target = self.lookup_label(label)

                    if self.curr is None:
                        continue

                    to_target = self.immutlink(self.curr, target, transform=t, execid_fork=True)
                    self.mutlink(self.curr, target, to_target, hot=t(self.curr.en), fixup_imprints=True)
            elif stat[0] == "quit":
                self.rewind("quit")
            elif stat[0] == "func":
                self.d.register_func(self.frame, stat)
            elif stat[0] == "chan":
                self.d.register_chan(self.frame, stat)
            elif stat[0] == "for":
                with stat as (_, cond, body):
                    have_cond = cond is not None
                    depart = self.curr

                    if hint_pre("period"):
                        try:
                            period_xform = FixedOffset(self.d.rtl_module, -int(hint_pre("period")))
                        except ValueError:
                            raise BadInput("bad period hint")
                    else:
                        period_xform = Id()

                    try:
                        unroll = int(hint_pre("unroll") or 1)
                        if unroll < 1:
                            raise ValueError(unroll)
                    except ValueError:
                        raise BadInput("bad unroll hint")

                    arrive = self.rewind()
                    self.rewind_to(depart)

                    body_replicas = [BlockSeq.from_ast_body(self.d, body, self.frame,
                                     inject_labels={"%break": arrive})
                                     for _ in range(unroll)]
                    cnodes = [None] * unroll
                    if have_cond:
                        csigs = [None] * unroll
                        with cond:
                            for rib in range(unroll):
                                label = f"{markers_str(Tuple.curr_markers)}_condition" \
                                        + (f"_rib_{rib}" if unroll > 1 else "")
                                cnodes[rib] = self.rewind(label=label)
                                csigs[rib] = csig = self.eval_cond(cond)
                                to_body_entry = self.immutlink(self.curr, body_replicas[rib].entry, hot=csig)
                                self.mutlink(self.curr, body_replicas[rib].entry,
                                             to_body_entry, hot=rtl.AND(m, csig, self.curr.en))

                        label = (f"{markers_str(Tuple.curr_markers)}_first_condition").replace(" ", "")
                        first_cnode = self.rewind(label=label)
                        self.mutlink(depart, self.curr, self.immutlink(depart, self.curr), hot=rtl.HIGH)
                        first_csig = self.eval_cond(cond)
                        to_body_entry = self.immutlink(self.curr, body_replicas[0].entry, hot=first_csig)
                        self.mutlink(self.curr, body_replicas[0].entry, to_body_entry,
                                     hot=rtl.AND(m, first_csig, first_cnode.en))
                    else:
                        csigs = [rtl.HIGH] * unroll
                        first_cnode = None
                        first_csig = rtl.HIGH
                        cnodes = [body_replicas[rib].entry for rib in range(unroll)]

                    for rib in range(unroll):
                        next_rib_entry = cnodes[(rib + 1) % unroll]
                        to_next_entry = self.immutlink(body_replicas[rib].exit, next_rib_entry,
                                                       transform=period_xform)
                        self.mutlink(body_replicas[rib].exit, next_rib_entry, to_next_entry,
                                     hot=period_xform(body_replicas[rib].exit.en, True))

                    self.rewind_to(arrive)
                    to_arrive = self.immutlink(first_cnode, self.curr,
                                               hot=rtl.NOT(m, first_csig))
                    self.mutlink(first_cnode, self.curr, to_arrive,
                                 hot=rtl.AND(m, first_cnode.en, rtl.NOT(m, first_csig)))

                    for rib in range(unroll):
                        node = cnodes[rib]
                        cond_to_curr = self.immutlink(node, self.curr,
                                                      hot=rtl.NOT(m, csigs[rib]))
                        self.mutlink(node, self.curr, cond_to_curr,
                                     hot=rtl.AND(m, node.en, rtl.NOT(m, csigs[rib])))
            elif stat[0] == "{":
                with stat as (_, body):
                    bseq = BlockSeq.from_ast_body(self.d, body, self.frame)

                    depart = self.curr
                    to_entry = self.immutlink(self.curr, bseq.entry)
                    self.mutlink(self.curr, bseq.entry, to_entry, hot=rtl.HIGH)

                    self.rewind()

                    if hint_pre("detach"):
                        to_arrive = self.immutlink(depart, self.curr)
                        self.mutlink(bseq.exit, self.curr,
                                     bseq.runthrough.inv * to_entry.inv * to_arrive, hot=rtl.HIGH)
                    else:
                        to_arrive = self.immutlink(bseq.exit, self.curr)
                        self.mutlink(bseq.exit, self.curr, to_arrive, hot=rtl.HIGH)
            elif stat[0] == "const":
                continue
            else:
                raise NotImplementedError(stat)


def escape_id(s):
    if not (s.startswith("$") or s.startswith("\\")):
        return f"\\{s}"
    else:
        return s


def fixup_goto_imprints(self):
    common = Frame.common_parent(self.tail.f, self.head.f)
    level = len(list(common.stack))
    self.imprint_pop = []
    for f in list(reversed(list(self.head.f.stack)))[level:]:
        if f.is_function:
            self.imprint_pop.append(MutLink.MATCH_NONE)
    self.imprint_push = []
    for f in list(reversed(list(self.tail.f.stack)))[level:]:
        if f.is_function:
            self.imprint_push.append(MutLink.MATCH_ANY)


class Design:
    def __init__(self, rtl_module):
        self._consts = {}
        self._function_asts = {}
        self.eval_const = ConstExprEvaluator(self._consts)
        self.blockimpls = set()
        self.funcseqs = dict()
        self.rtl_module = rtl_module
        self.create_clk_rst_ports()
        self.execid_width = 128
        self._ids_counter = 0

    def make_up_id(self):
        self._ids_counter += 1
        return self._ids_counter

    def create_clk_rst_ports(self):
        m = self.rtl_module
        self.rtl_clk = m.add_wire("\\clk", 1)
        self.rtl_clk.yw.port_input = True
        self.rtl_module.add_port(self.rtl_clk)
        self.rtl_rst = m.add_wire("\\rst", 1)
        self.rtl_rst.yw.port_input = True
        self.rtl_module.add_port(self.rtl_rst)

    def read_constants(self, nodes):
        for constdecl in nodes:
            with constdecl as (_1, ident, expr):
                self._consts[ident] = self.eval_const(expr)

    def get_function_ast(self, name):
        frame, ast = self._function_asts[name]
        return ast

    def eval_shape(self, shapenode):
        # TODO: evict mutable
        dims, signed, mutable = shapenode
        return Shape(*[self.eval_const(d) for d in dims], signed=signed), \
                mutable

    def _impl_crank(self, top_seq):
        m = self.rtl_module
        crank = m.add_wire("\\crank", 1)
        m.add_cell("$sdff",
            ("\\D", rtl.LOW),
            ("\\Q", crank),
            ("\\CLK", self.rtl_clk),
            ("\\SRST", self.rtl_rst),
            CLK_POLARITY=True,
            SRST_POLARITY=True,
            WIDTH=1,
            SRST_VALUE=True,
        )
        top_seq.entry.en_sources.append(crank)
        execid_zero = execid.ZERO(m, crank, width=self.execid_width)
        top_seq.entry.execid_sources.append(execid.DESCEND(m, execid_zero, crank))

        m.add_cell("\\VAR_SET",
            ("\\D", execid_zero),
            ("\\EN", rtl.HIGH),
            WIDTH=execid_zero.width,
            AT_NODE=top_seq.entry.id,
            NAMESPACE=top_seq.frame.namespace,
            NAME=f"$execid",
        )

    def impl_top_body(self, body):
        seq = BlockSeq(self)
        self.top_frame = seq.frame
        seq.impl_ast_body(body)
        seq.finalize()
        self._impl_crank(seq)

    def _impl_func(self, opname, name):
        assert opname in self._function_asts

        parent, ast = self._function_asts[opname]
        with ast as (_1, _2, argsdecl, retsdecl, body):
            if hint_pre("io"):
                if body != []:
                    raise BadInput("io functions must have empty body")
                body = parse_spec_from_buffer('''
                    var _valid [1] mut;
                entry:
                    # (1) set port valid signal
                    # (2) set ports to argument values
                    # (3) set valid variable according to port

                q:
                    if _valid {
                    feed:
                        # (4) set return values according to ports
                    }
                    delay(1);
                    if !_valid {
                        goto entry;
                    }
                ''', "compiler internal")
            seq = BlockSeq.from_ast_body(self, body,
                injectvars=argsdecl + retsdecl,
                parent_frame=parent,
                framename=name,
                function=True)
            if hint_pre("io"):
                m = self.rtl_module
                ovalid = m.add_wire(f"\\{name}_ovalid", 1)
                ovalid.yw.port_output = True
                entry_bimpl = seq.lookup_label("entry")
                m.connect(ovalid, entry_bimpl.en) # (1)
                for argname, shapenode in argsdecl: # (2)
                    shape, mutable = self.eval_shape(shapenode)
                    argwire = m.add_wire(f"\\{name}_{argname}", shape.bitlen)
                    argwire.yw.port_output = True
                    m.add_port(argwire)
                    if hint_pre("ahead"):
                        m.connect(argwire, rtl.SEER(m, seq.frame.vars[argname].eval(entry_bimpl)
                                                        .extract_underlying_signal(), 1))
                    else:
                        m.connect(argwire, seq.frame.vars[argname].eval(entry_bimpl)
                                                .extract_underlying_signal())
                ivalid = m.add_wire(f"\\{name}_ivalid", 1)
                ivalid.yw.port_input = True
                m.add_port(ivalid)
                seq.frame.vars["_valid"].assign(entry_bimpl, SignalValue(ivalid, Shape(1))) # (3)

                feed_bimpl = seq.lookup_label_in_children("feed")

                for retname, shapenode in retsdecl: # (4)
                    shape, mutable = self.eval_shape(shapenode)
                    retwire = m.add_wire(f"\\{name}_{retname}", shape.bitlen)
                    retwire.yw.port_input = True
                    m.add_port(retwire)
                    seq.frame.vars[retname].assign(feed_bimpl, SignalValue(retwire, shape))
            if hint_pre("blackbox"):
                if body != []:
                    raise BadInput("blackboxes must have empty body")
                module_name = hint_pre("blackbox") if type(hint_pre("blackbox")) is str \
                                                   else name
                conns = [
                    ("\\en", seq.entry.en),
                    ("\\clk", self.rtl_clk),
                ]
                for argname, shapenode in argsdecl:
                    shape, mutable = self.eval_shape(shapenode)
                    conns.append(
                        (f"\\{argname}", seq.frame.vars[argname].eval(seq.entry).extract_signal())
                    )
                for retname, shapenode in retsdecl:
                    shape, mutable = self.eval_shape(shapenode)
                    retwire = self.rtl_module.add_wire(f"${retname}", shape.bitlen)
                    seq.frame.vars[retname].assign(seq.entry, SignalValue(retwire, shape))
                    conns.append(
                        (f"\\{retname}", retwire)
                    )
                self.rtl_module.add_cell_keep(f"\\{module_name}", *conns)
            self.funcseqs[name] = seq

    def register_chan(self, frame, ast):
        if frame != self.top_frame:
            raise BadInput("channels can only be declared in the top scope")
        m = self.rtl_module
        with ast as (_1, name, lhsdecl, rhsdecl):
            avalid_i = m.add_wire(f"\\{name}_avalid_i", 1)
            avalid_o = m.add_wire(f"\\{name}_avalid_o", 1)
            bvalid_i = m.add_wire(f"\\{name}_bvalid_i", 1)
            bvalid_o = m.add_wire(f"\\{name}_bvalid_o", 1)
            a = [avalid_i]
            ay = [avalid_o]
            b = [bvalid_i]
            by = [bvalid_o]
            for varname, shapenode in lhsdecl:
                shape, mutable = self.eval_shape(shapenode)
                a.append(m.add_wire(f"\\{name}_a_{varname}_i", shape.bitlen))
                ay.append(m.add_wire(f"\\{name}_a_{varname}_o", shape.bitlen))
            for varname, shapenode in rhsdecl:
                shape, mutable = self.eval_shape(shapenode)
                b.append(m.add_wire(f"\\{name}_b_{varname}_i", shape.bitlen))
                by.append(m.add_wire(f"\\{name}_b_{varname}_o", shape.bitlen))
            rtl.TIMEPORTAL(m,
                rtl.concat(*a), rtl.concat(*ay),
                rtl.concat(*b), rtl.concat(*by)
            )
            for opname, leftward in zip([f"{name}->$nonblocking",
                                         f"{name}<-$nonblocking"], [False, True]):
                opast = Tuple("func", opname, 
                    [Tuple("%ivalid", Tuple([Const(1)], False, False))] + (rhsdecl if leftward else lhsdecl),
                    [Tuple("%ovalid", Tuple([Const(1)], False, False))] + (lhsdecl if leftward else rhsdecl),
                    []
                )
                self._function_asts[opname] = (frame, opast)
                seq = BlockSeq.from_ast_body(self, [], injectvars=opast[2] + opast[3],
                                             framename=opname, function=True,
                                             parent_frame=self.top_frame)
                seq.entry._label = opname
                i, o = (a, by) if not leftward else (b, ay)
                for sig, arg in zip(i, opast[2]):
                    argname, shapenode = arg
                    shape, mutable = self.eval_shape(shapenode)
                    val = seq.frame.vars[argname].eval(seq.entry)
                    if argname == "%ivalid":
                        m.connect(sig, rtl.AND(m, seq.entry.en, val.extract_signal()))
                    else:
                        m.connect(sig, val.extract_underlying_signal())
                for sig, ret in zip(o, opast[3]):
                    retname, shapenode = ret
                    shape, mutable = self.eval_shape(shapenode)
                    seq.frame.vars[retname].assign(seq.entry, SignalValue(sig, shape))
                self.funcseqs[opname] = seq
            for opname, leftward in zip([f"{name}->",
                                         f"{name}<-"], [False, True]):
                argsdecl = rhsdecl if leftward else lhsdecl
                retsdecl = lhsdecl if leftward else rhsdecl

                argvars = []
                for varname, shapenode in argsdecl:
                    shape, mutable = self.eval_shape(shapenode)
                    assert not mutable
                    argvars.append((varname, shape))
                retvars = []
                for varname, shapenode in retsdecl:
                    shape, mutable = self.eval_shape(shapenode)
                    assert not mutable
                    retvars.append((varname, shape))
                argset = ', '.join(varname for varname, shape in argvars)
                tmpdecl = ', '.join(f"{varname}_tmp {shape!s} mut" for varname, shape in retvars)
                tmpset = ', '.join(f"{varname}_tmp" for varname, shape in retvars)
                retset = ', '.join(varname for varname, shape in retvars)
                def comma(a):
                    return f", {a}" if a != "" else ""
                body_ast = parse_spec_from_buffer(f'''
                        var _valid [1] mut{comma(tmpdecl)};
                    entry:
                        _valid{comma(tmpset)} = {opname}$nonblocking(1{comma(argset)});
                    q:
                        if _valid {{
                        feed:
                            {f"{retset} = {tmpset};" if retset != "" else ""}
                        }}
                        delay(1);
                        if !_valid {{
                            goto entry;
                        }}
                ''', f"implementing '{opname}'")
                opast = Tuple("func", opname, argsdecl, retsdecl, body_ast)
                self._function_asts[opname] = (frame, opast)

    def register_func(self, frame, ast):
        self._function_asts[ast[1]] = (frame, ast)

    # TODO: @reentry_guard
    def request_func_blockseq(self, opname):
        try:
            parent, ast = self._function_asts[opname]
        except KeyError:
            raise BadInput("no such function: {:h}", opname)

        implname = opname

        with ast:
            if hint_pre("per_invocation"):
                no = 0
                while (implname := f"{opname}${no}") in self.funcseqs:
                    no += 1

        if implname not in self.funcseqs:
            self._impl_func(opname, implname)
        return self.funcseqs[implname]

    def do_pass(self, xfrm):
        for bi in self.blockimpls:
            bi.do_pass(xfrm)

    def build(self):
        for f in self.all_frames:
            f.immutlinks.finalize()

        for f in self.all_frames:
            f.build()
        for bi in self.blockimpls:
            bi.build()

        namespaces = dict()

        for f in self.all_frames:
            ns = list(reversed([escape_id(f_.framename) for f_ in f.stack]))[1:]
            for edge in f.immutlinks.edges:
                namespaces[edge] = ns
                namespaces[edge.rev] = ns
            for node in f.immutlinks.nodes:
                namespaces[node] = ns

        d = "immutlinks\n"
        for node in self.top_frame.immutlinks.nodes:
            d += f"node {node.id}\n"
            d += f"\tns {' '.join(namespaces[node] + [''])}end\n"
            assert isinstance(node.en, rtl.Wire)
            d += f"\ten {escape_id(node.en.name)} end\n"
            if node.src != "":
                escaped_src = node.src.replace("'", "\\'")
                d += f"\tsrc '{escaped_src}'\n"
            d += f"end\n"
        for edge in self.top_frame.immutlinks.edges:
            d += f"edge {edge.ep1.id} {edge.ep2.id}\n"
            d += f"\tns {' '.join(namespaces[edge] + [''])}end\n"
            assert edge.hot_back is rtl.LOW
            if not (isinstance(edge.hot, rtl.Const) and edge.hot.bits == [rtl.BitState.S1]):
                if edge.hot == rtl.LOW:
                    zwire = self.rtl_module.add_wire("$zero", 1)
                    self.rtl_module.connect(zwire, rtl.LOW)
                    d += f"\thot {escape_id(zwire.name)} end\n"
                else:
                    assert isinstance(edge.hot, rtl.Wire)
                    d += f"\thot {escape_id(edge.hot.name)} end\n"

            if isinstance(edge._xfer, Id):
                pass
            elif isinstance(edge._xfer, FixedOffset):
                d += f"\toffset {edge._xfer.offset}\n"
            elif isinstance(edge._xfer, BlockSeqTransform):
                xfer = edge._xfer
                assert not xfer.inversed
                d += f"\tbseq {xfer.bseq.entry.id} {xfer.bseq.exit.id}\n"
            else:
                raise NotImplementedError(type(edge._xfer))

            if edge.threshold is not None:
                d += f"\tphantom_threshold {edge.threshold.depth}\n"

            elem = self.top_frame.immutlinks._link_fg_eles[edge]

            if elem == Identity():
                d += f"\tin_spantree\n"
            else:
                d += f"\tfg_element '{elem.label}'\n"

            d += f"end\n"
        self.rtl_module.ym.design.scratchpad_set_string("immutlinks", d)

    @property
    def all_frames(self):
        qu = set([self.top_frame])

        while qu:
            f = qu.pop()
            yield f
            for ch in f.children_frames:
                qu.add(ch)


def do_dumps(args, d):
    if args.dump_immutlinks:
        # should we crash we don't want to overwrite the target
        buf = io.StringIO()
        d.top_frame.immutlinks.dump(buf)
        buf.seek(0)
        with open(args.dump_immutlinks, "w") as f:
            f.write(buf.read())

    if args.dump_mutlinks:
        import os
        if not os.path.exists(args.dump_mutlinks):
            os.path.mkdir(args.dump_mutlinks)
        for frame in d.all_frames:
            # should we crash we don't want to overwrite the target
            buf = io.StringIO()
            frame.mutlinks.dump(buf)
            buf.seek(0)
            path = "_".join(reversed([f.framename for f in frame.stack]))
            filename = f"{args.dump_mutlinks}/{path}"
            with open(filename, "w") as f:
                f.write(buf.read())
