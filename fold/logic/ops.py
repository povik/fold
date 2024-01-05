# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from inspect import getfullargspec
import io

from . import rtl
from ..ast import markers_str, Const, BadInput, Tuple
from ..utils import log2ceil

from .shape import SignalValue, Shape

SPECIAL_OPS = dict()


def register(name):
    def _register(f):
        SPECIAL_OPS[name] = f
    return _register


def check_nargs(opname, args, expected_nargs):
    if len(args) == expected_nargs \
            or (type(expected_nargs) in (list, range) and len(args) in expected_nargs):
        return

    if expected_nargs == 1:
        raise BadInput("{:h} expects one argument, given {}",
                       opname, len(args))
    elif type(expected_nargs) is int:
        raise BadInput("{:h} expects {} arguments, given {}",
                       opname, expected_nargs, len(args))
    else:
        raise BadInput("bad number of arguments to {:h}: {}",
                       opname, len(args))


def register_simple(name):
    def _register(f):
        spec = getfullargspec(f)
        nargs = len(spec.args) - 1

        def wrapper(bseq, expr):
            if spec.varargs is None:
                check_nargs(name, expr.args, nargs)
            return f(bseq, *[bseq.eval(node) for node in expr.args])
        SPECIAL_OPS[name] = wrapper
    return _register


def _print_transform_fmt(fmtstring, args):
    args = list(args)
    fmtbuf = io.BytesIO(fmtstring)
    ret = b""
    while (ch := fmtbuf.read(1)) != b"":
        if ch == b"%":
            verb = fmtbuf.read(1)
            if verb == b"%":
                ret += verb
                continue
            align = b">"
            pad = b" "
            width = 0
            # flags
            while True:
                if verb == b"-":
                    align = b"<"
                elif verb == b"0":
                    pad = b"0"
                else:
                    break
                verb = fmtbuf.read(1)
            # width
            while verb != b"" and verb in b"0123456789":
                width = 10 * width + int(verb)
                verb = fmtbuf.read(1)
            # type
            if verb != b"" and verb in b"bodch":
                typech = verb
            elif verb == b"x":
                typech = b"h"
            elif verb == b"":
                raise BadInput("unfinished format specifier")
            else:
                raise BadInput("bad character in format specifier: f{!r:h}", typech)
            if not len(args):
                raise BadInput("missing data argument for a formatting verb")
            arg = args.pop(0)
            signedness = b"s" if arg.shape.signed else b"u"
            ret += b"{" + b"%d" % arg.shape.bitlen + b":" + align + pad + \
                        (b"%d" % width if width != 0 else b"") + typech + \
                        (signedness if verb != b"c" else b"") + b"}"
        elif ch == b"{":
            ret += b"{{"
        elif ch == b"}":
            ret += b"}}"
        else:
            ret += ch
    if len(args):
        raise BadInput("extra data arguments lacking a formatting verb")
    return ret


@register("print")
def print_op(bseq, expr):
    check_nargs("print", expr.args, range(1, 100)) # TODO: remove upper bound
    arg0 = bseq.eval(expr.args[0])

    with expr.args[0]:
        if not isinstance(arg0, SignalValue) or arg0.shape.ndims != 2 or arg0.shape.dims[1] != 8 \
                or not isinstance(arg0.extract_underlying_signal(), rtl.Const):
            raise BadInput("bad format string for print")
        fmtstring = arg0.extract_underlying_signal().bytes[1:-1]

    fmt_values = [bseq.eval(node) for node in expr.args[1:]]

    transformed_fmt = \
            _print_transform_fmt(fmtstring.replace(b"\\n", b"\n"),
                                 fmt_values)

    args = rtl.concat(*[a.extract_signal() for a in fmt_values])
    bseq.d.rtl_module.add_cell_keep("$print",
        ("\\TRG", bseq.d.rtl_clk),
        ("\\EN", bseq.curr.en),
        ("\\ARGS", args),
        FORMAT=transformed_fmt,
        ARGS_WIDTH=args.width,
        TRG_ENABLE=True,
        TRG_WIDTH=1,
        TRG_POLARITY=1,
        PRIORITY=0,
    )


@register_simple("assert_equal")
def assert_equal(bseq, a, b):
    a, b = Shape.cast_to_common(a, b)

    with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
        eq_sig = rtl.EQ(bseq.d.rtl_module,
            a.extract_underlying_signal(),
            b.extract_underlying_signal()
        )
        rtl.ASSERT(bseq.d.rtl_module, bseq.curr.en, eq_sig)


@register_simple("cover")
def cover(bseq, *args):
    check_nargs("covers", args, [0, 1])
    with rtl.SynthAttrContext(src=markers_str(Tuple.curr_markers)):
        if len(args):
            cond = rtl.REDUCE_BOOL(bseq.d.rtl_module, args[0].extract_signal())
        else:
            cond = rtl.HIGH
        rtl.COVER(bseq.d.rtl_module, bseq.curr.en, cond)


@register_simple("r")
def r(bseq, arg):
    m = bseq.d.rtl_module
    arg_signal = arg.extract_underlying_signal()
    ret = SignalValue(m.add_wire("$r", arg_signal.width), arg.shape)

    m.add_cell("$dff",
        ("\\D", rtl.SEER(m, arg_signal, 1)),
        ("\\Q", ret.extract_underlying_signal()),
        ("\\CLK", bseq.d.rtl_clk),
        CLK_POLARITY=True,
        WIDTH=arg_signal.width,
    )

    return ret


@register_simple("ctz")
def ctz(bseq, arg):
    m = bseq.d.rtl_module
    ret_shape = Shape(log2ceil(arg.shape.bitlen))
    sig, hot = rtl.build_mux(m, list(reversed([(rtl.Signal.from_bits([bit]), rtl.Signal.from_const(no, ret_shape.bitlen))
                                  for no, bit in enumerate(arg.extract_signal().bits)])),
                                  with_priority=True)
    return SignalValue(sig, ret_shape)


@register("__local_exec_id")
def __local_exec_id(bseq, expr, args):
    m = bseq.d.rtl_module
    ret = m.add_wire("$", bseq.d.execid_width)
    self.m.add_cell("\\VAR_GET",
        ("\\Q", ret),
        WIDTH=sig.width,
        AT_NODE=escape_id(bi.label),
        NAMESPACE=self.f.namespace,
        NAME=self.varname,
    )


@register("read_tsv!")
def read_tsv(bseq, expr):
    check_nargs("read_tsv!", expr.args, [1, 2])
    with expr.args[0] as arg:
        if not type(arg) is Const or type(arg.val) is not str:
            raise BadInput("bad filename argument for {:h}", "read_tsv!")
        filename = arg.val[1:-1]

    # TODO: checking
    data = bytearray()
    lines = 0
    columns = 0
    try:
        with open(bytes(filename, "ascii"), "r") as f:
            for line in f:
                if not line:
                    continue
                lines += 1
                columns = 0
                for val in line.split():
                    columns += 1
                    data += int(val).to_bytes(4, byteorder="little")
    except FileNotFoundError:
        raise BadInput("file not found: {:h}", filename)

    shape = Shape(lines, columns, 32, signed=True)
    return SignalValue(rtl.Signal.from_const(bytes(reversed(data))), shape)


def timeportal(m, sig):
    ret = m.add_wire("$$", sig.width)
    cell = m._add_cell("\\TIMEPORTAL",
        ("\\A", sig),
        ("\\AY", ret),
        ("\\B", rtl.Signal.from_bits([])),
        ("\\BY", rtl.Signal.from_bits([])),
        A_WIDTH=sig.width,
        B_WIDTH=0,
    )
    return ret


@register("__arbitrary")
def __arbitrary(bseq, expr):
    check_nargs("__arbitrary", expr.args, [0, 1])

    if len(expr.args) == 1:
        with expr.args[0] as arg:
            if not type(arg) is Const or type(arg.val) is not int:
                raise BadInput("bad argument to {:h}", "__arbitrary")
            width = arg.val
    else:
        width = 1

    m = bseq.d.rtl_module
    w = m.add_wire("\\arbitrary", width)
    w.yw.port_input = True
    return SignalValue(timeportal(m, w), Shape(width))


@register_simple("__requires")
def __requires(bseq, arg):
    sig = arg.extract_underlying_signal()
    m = bseq.d.rtl_module
    w = m.add_wire("\\requires", sig.width)
    w.yw.port_output = True
    m.connect(w, timeportal(m, sig))


@register_simple("__effectful")
def __effectful(bseq):
    m = bseq.d.rtl_module
    w = m.add_wire("\\__effectful", 1)
    w.yw.port_output = True
    m.connect(w, timeportal(m, bseq.curr.en))


@register_simple("__execid")
def __execid(bseq):
    s = bseq.curr.execid
    return SignalValue(s, Shape(s.width))


@register_simple("__frame_execid")
def __frame_execid(bseq):
    s = bseq.frame.frame_execid(bseq.curr)
    return SignalValue(s, Shape(s.width))
