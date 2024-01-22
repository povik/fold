# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

try:
    # running under yosys process
    import libyosys as ys
except ImportError:
    try:
        # running outside yosys process
        from fold_pyosys import libyosys as ys
    except ImportError:
        # running outside yosys process
        from pyosys import libyosys as ys
from enum import Enum
import unittest
import sys
import os.path

from functools import cache


class BitState(Enum):
    S0 = 0
    S1 = 1
    Sx = 2


class Design:
    def __init__(self, yd=None):
        self.yd = yd or ys.Design()

    @classmethod
    def from_ys(self, yd):
        return Design(yd)

    def add_module(self, name):
        return Module(self.yd.addModule(ys.IdString(name)))

    def write_il(self, fn):
        ys.run_pass(f"write_rtlil {fn}", self.yd)


ID_Y_WIDTH = ys.IdString("\\Y_WIDTH")
ID_WIDTH = ys.IdString("\\WIDTH")
ID_FOLDSPEC_SRC = ys.IdString("\\foldspec_src")


def _to_idstring(s):
    s = s.replace(' ', '_') # TODO
    assert s.startswith("$") or s.startswith("\\")
    return ys.IdString(s)


class SynthAttrContext:
    _ATTR_STACK = set()

    def __init__(self, **attrs):
        self.attrs = attrs

    def __enter__(self):
        self._ATTR_STACK.add(self)
        return self

    def __exit__(self, *args):
        self._ATTR_STACK.remove(self)
        return

    @classmethod
    def injected_attrs(self):
        return ((k, v) for attrs in self._ATTR_STACK for k, v in attrs.attrs.items())


class Cell:
    def __init__(self, yc):
        self.yc = yc

    @classmethod
    def _paramval_const(self, val):
        if type(val) is bytes:
            return ys.Const(val)
        elif type(val) is str:
            return ys.Const(val.encode('ascii'))
        elif type(val) is int:
            if val >= 0:
                bitlen = val.bit_length() + 1
            else:
                bitlen = (-val - 1).bit_length() + 1
            return ys.Const(val, bitlen + 1)
        elif type(val) is bool:
            return ys.Const(val, 1)
        elif type(val) is Const:
            return val.ss.as_const()
        else:
            raise NotImplementedError(type(val))

    @property
    def name(self):
        return self.yc.name.str()

    def append_on_port(self, portname, sig):
        portid = _to_idstring(portname)
        portsig = self.yc.getPort(portid)
        portsig.append(sig.ss)
        self.yc.setPort(portid, portsig)

    def get_port(self, portname):
        return Signal._from_sigspec(self.yc.getPort(_to_idstring(portname)))

    def set_port(self, portname, sig):
        self.yc.setPort(_to_idstring(portname), sig.ss)

    def set_param(self, paramname, val):
        paramid = _to_idstring(f"\\{paramname}")
        self.yc.setParam(paramid, self._paramval_const(val))


def encode_string(str_):
    encoded = str_.encode("ascii")
    return Signal._from_sigspec(ys.SigSpec(ys.Const(encoded)))

    #return Signal.from_bits([
    #   BitState.S1 if ((c >> i) & 1) else BitState.S0
    #   for c in encoded for i in range(8)
    #])


def _convert_to_const(v):
    if isinstance(v, bool):
        return Const.from_const(int(v), 1)
    elif isinstance(v, str):
        return encode_string(v)
    elif isinstance(v, Const):
        return v
    else:
        raise NotImplementedError(type(v))


class Module:
    def __init__(self, ym):
        self.ym = ym
        self.used_wires = set()
        self._attr_stack = set()
        self.counter = 0

    @property
    def name(self):
        return self.ym.name.str()

    def add_wire(self, name, width, **synth_attrs):
        if not width:
            return Signal.from_bits([])

        frame = sys._getframe(1)
        foldspec_src = f"{os.path.basename(frame.f_code.co_filename)}:{frame.f_lineno}"

        if name in self.used_wires:
            name = f"{name}${self.counter}"
            self.counter += 1

        self.used_wires.add(name)
        ywire = self.ym.addWire(_to_idstring(name), width)
        ywire.attributes = {
            ID_FOLDSPEC_SRC: encode_string(foldspec_src).ss.as_const(),
            **{ys.IdString(f"\\{k}"): _convert_to_const(v).ss.as_const() \
               for k, v in synth_attrs.items()}
        }
        return Signal._from_yosys_wire(ywire)

    def get_wire(self, name):
        assert name.startswith("$") or name.startswith("\\")

        if name not in self.used_wires:
            return None

        return Signal._from_yosys_wire(self.ym.wire(ys.IdString(name)))

    def add_memory(self, name, size, width):
        assert name.startswith("$") or name.startswith("\\")
        mem = ys.Memory()
        mem.size = size
        mem.width = width
        ymem = self.ym.addMemory(ys.IdString(name), mem)
        for k, v in SynthAttrContext.injected_attrs():
            if isinstance(v, bool):
                ymem.set_bool_attribute(ys.IdString(f"\\{k}"), v)
            elif isinstance(v, str):
                ymem.set_string_attribute(ys.IdString(f"\\{k}"), v)
            else:
                raise NotImplementedError(type(v))

    def add_port(self, wire):
        assert type(wire) is Wire
        self.ym.ports.append(wire.yw.name)

    def _add_cell(self, celltype, *args, **params):
        cell = self.ym.addCell(ys.new_id("", 1, ""),
                               ys.IdString(celltype))
        for key, val in params.items():
            cell.setParam(ys.IdString(f"\\{key}"),
                          Cell._paramval_const(val))

        for i in range(5):
            frame = sys._getframe(i)
            if "rtl.py" not in frame.f_code.co_filename:
                break
        foldspec_src = f"{os.path.basename(frame.f_code.co_filename)}:{frame.f_lineno}"

        cell.set_string_attribute(ID_FOLDSPEC_SRC, foldspec_src)

        for k, v in SynthAttrContext.injected_attrs():
            if isinstance(v, bool):
                cell.set_bool_attribute(ys.IdString(f"\\{k}"), v)
            elif isinstance(v, str):
                cell.set_string_attribute(ys.IdString(f"\\{k}"), v)
            else:
                raise NotImplementedError(type(v))

        if all(type(arg) is tuple for arg in args):
            for key, signal in args:
                cell.setPort(ys.IdString(key), signal.ss)
        elif all(isinstance(arg, Signal) for arg in args):
            for arg, key in zip(args, ["\\A", "\\B", "\\C", "\\D"]):
                cell.setPort(ys.IdString(key), arg.ss)
        else:
            raise ValueError("bad arguments")

        return cell

    def add_cell(self, celltype, *args, **params):
        # here we drop the return value of _add_cell
        cell = self._add_cell(celltype, *args, **params)
        cell.check()
        return Cell(cell)

    def add_cell_keep(self, celltype, *args, **params):
        # here we drop the return value of _add_cell
        cell = self._add_cell(celltype, *args, **params)
        cell.set_bool_attribute(ys.IdString("\\keep"), True)
        cell.check()
        return Cell(cell)

    #TODO: have per-module cache
    #TODO: what about synth attributes? those are not considered
    # by the cache
    @cache
    def operate(self, opname, *args, **params):
        cell = self._add_cell(opname, *args, **params)

        if not cell.hasParam(ID_Y_WIDTH):
            if opname in ["$add", "$not", "\\SEER"]:
                result_width = max(arg.width for arg in args)
            elif opname in ["$mul"]:
                result_width = sum(arg.width for arg in args)
            elif opname in ["$and", "$or"]:
                result_width = min(arg.width for arg in args)
            elif opname in ["$gt", "$lt", "$reduce_or", "$reduce_and", "$reduce_bool"]:
                result_width = 1
            elif opname in ["$pmux", "$mux"]:
                result_width = cell.getParam(ID_WIDTH).as_int(False)
            else:
                raise NotImplementedError(opname)
        else:
            result_width = cell.getParam(ID_Y_WIDTH).as_int(False)

        ret = self.add_wire(ys.new_id("", 1, "").str(), result_width)
        cell.setPort(ys.IdString("\\Y"), ret.ss)
        cell.fixup_parameters(False, False)
        cell.check()
        return ret

    def connect(self, lhs, rhs):
        assert lhs.width == rhs.width
        self.ym.connect(lhs.ss, rhs.ss)


def concat(*sigs):
    ret = ys.SigSpec()
    for sig in sigs:
        ret.append(sig.ss)
    return Signal._from_sigspec(ret)


def repeat(sig, nrepeats):
    return concat(*([sig] * nrepeats))


def label_signal(m, sig, label):
    ret = m.add_wire(label, sig.width)
    m.connect(ret, sig)
    return ret


def build_mux(m, cases, width=None, with_priority=False, zeroed=False):
    if width is None:
        widths = set([sig.width for _, sig in cases])
        assert len(widths) == 1
        width = widths.pop()
    else:
        assert all((sig.width == width for _, sig in cases))

    default_bitstate = BitState.S0 if zeroed else BitState.Sx

    cases = [
        (hot, val) for hot, val in cases if hot != LOW
    ]

    if not len(cases):
        return Signal.from_bits([default_bitstate] * width), \
                Signal.from_const(0, 1)

    if not with_priority:
        val = m.operate("$pmux",
            ("\\A", Signal.from_bits([default_bitstate] * width)),
            ("\\B", concat(*[sig for _, sig in cases])),
            ("\\S", concat(*[hot for hot, _ in cases])),
            WIDTH=width,
        )
    else:
        val = Signal.from_bits([default_bitstate] * width)
        for case_hot, case_val in cases:
            val = m.operate("$mux",
                ("\\A", val),
                ("\\B", case_val),
                ("\\S", case_hot),
                WIDTH=width,
            )

    hot = m.operate("$reduce_or", concat(*[hot for hot, _ in cases]))

    return val, hot


def adjust_width(sig, target_width):
    ret = Signal.from_bits(sig.bits[:target_width] + \
                            [BitState.S0] * (target_width - sig.width))
    assert ret.width == target_width
    return ret


class WireBit:
    def __init__(self, wire, pos):
        self.wire, self.pos = wire, pos

    def __eq__(self, other):
        return self.wire == other.wire and \
                self.pos == other.pos

    def __repr__(self):
        return f"<Bit {self.pos} of wire {self.wire.name}>"


class Signal:
    def __init__(self, ss):
        assert type(ss) is ys.SigSpec
        self.ss = ss
        if type(self) is Signal:
            assert not ss.is_fully_const() \
                   and not ss.is_wire()

    @classmethod
    def _from_sigspec(self, ss):
        if ss.is_fully_const():
            return Const(ss)
        elif ss.is_wire():
            return Wire(ss)
        else:
            return Signal(ss)

    @classmethod
    def _from_yosys_wire(self, yw):
        w = self._from_sigspec(ys.SigSpec(yw))
        assert type(w) is Wire
        return w

    @classmethod
    def _to_yosys_bit(self, bit):
        if type(bit) is BitState:
            return ys.SigChunk({
                BitState.S0: ys.State.S0,
                BitState.S1: ys.State.S1,
                BitState.Sx: ys.State.Sx,
            }[bit], 1)
        elif type(bit) is WireBit:
            return ys.SigChunk(
                bit.wire.ss.at(bit.pos, ys.SigBit(ys.State.Sx)))

    @classmethod
    def _from_yosys_bit(self, bit):
        if bit.is_wire():
            return WireBit(self._from_yosys_wire(bit.wire),
                           bit.offset)
        else:
            return {
                ys.State.S0: BitState.S0,
                ys.State.S1: BitState.S1,
                ys.State.Sx: BitState.Sx,
            }[bit.data]

    @classmethod
    def from_bits(self, bits):
        assert all((type(b) in [BitState, WireBit]\
                    for b in bits))
        return self._from_sigspec(ys.SigSpec([
            Signal._to_yosys_bit(b) \
            for b in bits
        ]))

    @classmethod
    def from_const(self, val, width=None):
        if type(val) is bytes:
            assert width is None
            return self._from_sigspec(ys.SigSpec(ys.Const(val)))

        if width is None:
            width = val.bit_length()

        bits = [
            BitState.S1 if (val & (1 << i)) else BitState.S0
            for i in range(width)
        ]
        return self.from_bits(bits)
        #return self._from_sigspec(ys.SigSpec(val, width))

    @property
    def bits(self):
        return [
            self._from_yosys_bit(b)
            for b in self.ss.bits()
        ]

    @property
    def width(self):
        return self.ss.size()

    def slot(self, width, i):
        return self[width*i:width*(i+1)]

    def pad_to(self, width, padbit=BitState.S0):
        return concat(self, self.from_bits([padbit] * (width - self.width)))

    def __hash__(self):
        return hash(self.ss)

    def __eq__(self, other):
        return type(self) is type(other) \
               and self.ss == other.ss

    def __getitem__(self, spec):
        if type(spec) is int:
            return self.bits[spec]
        elif type(spec) is slice:
            #return Signal.from_bits(self.bits[spec])
            start, stop = spec.start, spec.stop
            if start is None:
                start = 0
            if stop is None or stop > self.width:
                stop = self.width
            return Signal._from_sigspec(self.ss.extract(start, stop - start))


class Const(Signal):
    def __init__(self, *args):
        super().__init__(*args)
        assert self.ss.is_fully_const()

    @property
    def value(self):
        return self.ss.as_int()

    @property
    def bytes(self):
        return bytes(self.ss.as_const().decode_string(), "ascii") # TODO

    def __repr__(self):
        return f"Const {self.bits!r}"


class Wire(Signal):
    def __init__(self, *args):
        super().__init__(*args)
        assert self.ss.is_wire()
        self.yw = self.ss.chunks()[0].wire

    @property
    def name(self):
        return self.yw.name.str()

    def __str__(self):
        return f"Wire {self.name}"

    def __repr__(self):
        return f"<{str(self)}>"


HIGH = Signal.from_const(1, 1)
LOW = Signal.from_const(0, 1)
UNDEF = Signal.from_bits([BitState.Sx])


def same(*args):
    return len(set(args)) <= 1


def AND(m, *opers):
    assert same(len(sig) for sig in opers)

    if LOW in opers:
        return LOW

    if len(opers) == 2:
        if True:
            if opers[0] == LOW or opers[1] == LOW:
                return LOW
            if opers[0] == HIGH:
                return opers[1]
            if opers[1] == HIGH:
                return opers[0]

        return m.operate("$and", *opers)
    elif len(opers) == 0:
        return HIGH
    elif opers[0].width == 1:
        return m.operate("$reduce_and", concat(*opers))
    else:
        shape = [len(sig) for sig in opers]
        raise NotImplementedError(shape)


def OR(m, *opers):
    assert same(len(sig) for sig in opers)

    if HIGH in opers:
        return HIGH

    if len(opers) == 2:
        if True:
            if opers[0] == HIGH or opers[1] == HIGH:
                return HIGH
            if opers[0] == LOW:
                return opers[1]
            if opers[1] == LOW:
                return opers[0]
        return m.operate("$or", *opers)
    elif len(opers) == 0:
        return LOW
    elif opers[0].width == 1:
        return m.operate("$reduce_or", concat(*opers))
    else:
        shape = [len(sig) for sig in opers]
        raise NotImplementedError(shape)


def NOT(m, oper):
    return m.operate("$not", oper)


def SEER(m, sig, offset, backwards=False):
    if backwards:
        ret = m.add_wire("$seer", sig.width)
        m.connect(sig, m.operate("\\SEER", ret, WIDTH=sig.width, OFFSET=offset))
        return ret
    if isinstance(sig, Const):
        return sig
    return m.operate("\\SEER", sig, WIDTH=sig.width, OFFSET=offset)


def ASSERT(m, en, a):
    m.add_cell_keep("$assert",
        ("\\EN", en),
        ("\\A", a)
    )


def COVER(m, en, a):
    m.add_cell_keep("$cover",
        ("\\EN", en),
        ("\\A", a)
    )


def ADD(m, a, b, reswidth=None):
    reswidth = reswidth or (max(a.width, b.width) + 1)
    return m.operate("$add", a, b,
        Y_WIDTH=reswidth,
        A_SIGNED=False,
        B_SIGNED=False,
    )


def MUL(m, a, b, reswidth=None):
    reswidth = reswidth or (a.width + b.width)
    return m.operate("$mul", a, b,
        Y_WIDTH=reswidth,
        A_SIGNED=False,
        B_SIGNED=False,
    )


def SHIFTX(m, a, b, reswidth=None):
    reswidth = reswidth or a.width
    return m.operate("$shiftx", a, b,
        Y_WIDTH=reswidth,
        A_SIGNED=False,
        B_SIGNED=False,
    )


def SHL(m, a, b, reswidth=None):
    reswidth = reswidth or a.width
    return m.operate("$shl", a, b,
        Y_WIDTH=reswidth,
        A_SIGNED=False,
        B_SIGNED=False,
    )


def MUX(m, a, b, s):
    assert a.width == b.width
    return m.operate("$mux",
        ("\\A", a),
        ("\\B", b),
        ("\\S", s),
        WIDTH=a.width,
    )


def EQ(m, a, b):
    return m.operate("$eq", a, b, Y_WIDTH=1)


def EQX(m, a, b):
    return m.operate("$eqx", a, b, Y_WIDTH=1)


def PMUX(m, sel, vals, width=None):
    assert all(sig.width == 1 for sig in sel)
    assert len(sel) == len(vals)
    widths = set([sig.width for sig in vals])
    if width is None:
        assert len(widths) == 1
        width = widths.pop()
    else:
        assert all(w == width for w in widths)
    return m.operate("$pmux",
        ("\\A", Signal.from_bits([BitState.Sx] * width)),
        ("\\B", concat(*vals)),
        ("\\S", concat(*sel)),
        WIDTH=width,
    )


def DFF(m, clk, d, rst=LOW, en=HIGH, rst_value=None):
    q = m.add_wire("$q", d.width)
    m.add_cell("$sdffe",
        ("\\CLK", clk),
        ("\\D", d),
        ("\\EN", en),
        ("\\Q", q),
        ("\\SRST", rst),
        CLK_POLARITY=True,
        EN_POLARITY=True,
        SRST_POLARITY=True,
        SRST_VALUE=rst_value or repeat(UNDEF, d.width),
        WIDTH=d.width,
    )
    return q


def MEMINIT(m, memid, en=None, addr=None, data=None, width=None):
    assert addr is not None
    assert data is not None
    assert width is not None
    assert data.width % width == 0
    m.add_cell("$meminit_v2",
        ("\\EN", en if en is not None else repeat(HIGH, width)),
        ("\\ADDR", addr),
        ("\\DATA", data),
        MEMID=memid,
        WIDTH=width,
        WORDS=data.width // width,
        ABITS=addr.width,
        PRIORITY=0,
    )


def MEMRD(m, memid, clk=None, en=None, addr=None, width=0,
          data=None, transparent=False, collision_x=False):
    if data is not None and width == 0:
        width = data.width
    assert width != 0 and addr is not None
    assert (clk is None and en is None) or (clk is not None and en is not None)
    assert en is None or en.width == 1
    if data is None:
        data = m.add_wire("$data", width)
    m.add_cell("$memrd_v2",
        ("\\CLK", clk if clk is not None else LOW),
        ("\\EN", en if en is not None else HIGH),
        ("\\ARST", LOW),
        ("\\SRST", LOW),
        ("\\ADDR", addr),
        ("\\DATA", data),
        MEMID=memid,
        CLK_ENABLE=clk is not None,
        CLK_POLARITY=True,
        TRANSPARENCY_MASK=transparent,
        COLLISION_X_MASK=collision_x,
        CE_OVER_SRST=False,
        WIDTH=width,
        ARST_VALUE=repeat(UNDEF, width),
        SRST_VALUE=repeat(UNDEF, width),
        INIT_VALUE=repeat(UNDEF, width),
        ABITS=addr.width,
    )
    return data


def MEMWR(m, memid, clk=None, en=None, addr=None, data=None,
          portid=0, priority_mask=0):
    assert en is not None
    assert addr is not None
    assert data is not None
    if en.width == 1 and en.width != data.width:
        en = repeat(en, data.width)
    assert en.width == data.width
    m.add_cell("$memwr_v2",
        ("\\CLK", clk if clk is not None else LOW),
        ("\\EN", en),
        ("\\ADDR", addr),
        ("\\DATA", data),
        MEMID=memid,
        CLK_ENABLE=clk is not None,
        CLK_POLARITY=True,
        PORTID=portid,
        PRIORITY_MASK=priority_mask,
        WIDTH=data.width,
        ABITS=addr.width,
    )


def PRINT(m, clk, en, fmt, arg):
    m.add_cell_keep("$print",
        ("\\TRG", clk),
        ("\\EN", en),
        ("\\ARGS", arg),
        FORMAT=fmt,
        ARGS_WIDTH=arg.width,
        TRG_ENABLE=True,
        TRG_WIDTH=1,
        TRG_POLARITY=1,
        PRIORITY=0,
    )


def replace_x_bits_w_wire(m, signal):
    assert isinstance(signal, Signal)
    bits = signal.bits
    retbits = []
    i = 0
    while i < len(bits):
        si = i
        while i < len(bits) and bits[i] is BitState.Sx:
            i += 1
        if i > si:
            retbits += m.add_wire("$dummy", i - si).bits
        if i < len(bits):
            retbits.append(bits[i])
            i += 1
    assert len(retbits) == signal.width
    return Signal.from_bits(retbits)


def REDUCE_BOOL(m, val):
    return m.operate("$reduce_bool", val)


def TIMEPORTAL(m, a, ay, b, by):
    assert a.width == ay.width
    assert b.width == by.width
    # TODO: should not be exposing the cell
    return m._add_cell("\\TIMEPORTAL",
        ("\\A", a),
        ("\\AY", ay),
        ("\\B", b),
        ("\\BY", by),
        A_WIDTH=a.width,
        B_WIDTH=b.width
    )

def wrap_in_timeportal(m, *ports, **kwargs):
    mapped_ports = []

    a, ay = Signal.from_bits([]), Signal.from_bits([])
    b, by = Signal.from_bits([]), Signal.from_bits([])

    for portname, signal, is_output in ports:
        signal_like = m.add_wire("$m", signal.width)
        mapped_ports.append((portname, signal_like))
        if is_output:
            b = concat(b, signal_like)
            by = concat(by, signal)
        else:
            a = concat(a, signal)
            ay = concat(ay, signal_like)

    cell = TIMEPORTAL(m, a, ay, b, by)
    for k, v in kwargs.items():
        cell.set_string_attribute(ys.IdString(f"\\{k}"), v)
    return mapped_ports


def wrap_in_timeportal2(m, bank_a_domain=None, bank_a_constraint=None,
                        bank_b_domain=None, bank_b_constraint=None, **ports):
    mapped_ports = {}

    a, ay = Signal.from_bits([]), Signal.from_bits([])
    b, by = Signal.from_bits([]), Signal.from_bits([])

    for portname, spec in ports.items():
        signal, is_output = spec
        signal_like = m.add_wire("$m", signal.width)
        mapped_ports[portname] = signal_like
        if is_output:
            b = concat(b, signal_like)
            by = concat(by, signal)
        else:
            a = concat(a, signal)
            ay = concat(ay, signal_like)

    cell = TIMEPORTAL(m, a, ay, b, by)

    if bank_a_domain is not None:
        cell.set_string_attribute(ys.IdString("\\timetravel_bank_a_domain"),
                                  bank_a_domain)
    if bank_a_constraint is not None:
        cell.set_string_attribute(ys.IdString("\\timetravel_bank_a_constraint"),
                                  bank_a_constraint)
    if bank_b_domain is not None:
        cell.set_string_attribute(ys.IdString("\\timetravel_bank_b_domain"),
                                  bank_b_domain)
    if bank_b_constraint is not None:
        cell.set_string_attribute(ys.IdString("\\timetravel_bank_b_constraint"),
                                  bank_b_constraint)

    return mapped_ports


def MUTEX_ASSERT(m, clk, a, message=""):
    if type(a) is list:
        a = concat(*a)
    m.add_cell_keep(
        "\\MUTEX_ASSERT",
        ("\\CLK", clk),
        ("\\A", a),
        WIDTH=a.width,
        MESSAGE=message,
    )


def PLACEHOLDER(m, width):
    ret = m.add_wire("$placeholder", width)
    m.add_cell(
        "\\PLACEHOLDER",
        ("\\Y", ret),
        WIDTH=ret.width
    )
    return ret


class TestSignals(unittest.TestCase):
    def test_to_bits_n_back(self):
        d = Design()
        m = d.add_module("\\testmodule")

        testsignals = [
            Signal.from_bits([BitState.S0, BitState.S1, BitState.Sx]),
            m.add_wire("\\testwire", 3),
        ]

        for sig in testsignals:
            sig_ = Signal.from_bits(sig.bits)
            self.assertEqual(sig_, sig)
            self.assertEqual(sig_.bits, sig.bits)


def main():
    unittest.main()


if __name__ == "__main__":
    main()
