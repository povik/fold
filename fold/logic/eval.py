# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import sys

from .. import ast
from ..eval import _ExprEvaluator
from ..utils import product

from .shape import SignalValue, Shape
from . import rtl

BASIC_OPS = ["~", "*", "+", "-", "<<", ">>",
             "&", "|", "==", "!=", "<", "<=",
             ">", ">=", "//", "{", "!",
             "&&", "||", "%", "?:", "^"]


class CombinatorialEvaluator(_ExprEvaluator):
    def __init__(self, rtl_module):
        self.m = rtl_module

    @classmethod
    def _broadcast(self, common_dims, common_signed, value):
        if common_signed and not value.shape.signed:
            value = value.cast(value.shape.signed_extend)

        if value.shape.dims[:-1] == common_dims:
            return value
        assert value.ndims == 1

        return SignalValue(
            rtl.concat(*([value.extract_signal()] * product(*common_dims))),
            Shape(common_dims + [value.shape.dims[0]], value.signed)
        )

    @classmethod
    def _broadcast_two(self, a, b):
        if a.shape.ndims > 1 and b.shape.ndims > 1 \
                and a.shape.dims[:-1] != b.shape.dims[:-1]:
            raise BadInput("shapes cannot be broadcasted")

        common_dims = a.shape.dims[:-1] if a.shape.ndims > 1 \
                      else b.shape.dims[:-1]

        common_signed = a.shape.signed or b.shape.signed

        a = self._broadcast(common_dims, common_signed, a)
        b = self._broadcast(common_dims, common_signed, b)
        return a, b

    def do_signal_op(self, celltype, shape_policy, *args):
        assert len(args) <= 2

        if len(args) == 2 and celltype not in ["$sshl", "$sshr"]:
            args = self._broadcast_two(*args)

        assert len(set(a.shape.dims[:-1] for a in args)) == 1

        retshape = shape_policy(*[val.shape.drop_dims(val.shape.ndims - 1)
                                  for val in args]).prepend_dims(args[0].shape.dims[:-1])
        arg_signals = [val.extract_underlying_signal() for val in args]

        widths = {
            prefix + "WIDTH": arg.shape.bitlen
            for arg, prefix in zip(args, ["A_", "B_", "S_"])
        }

        signednesses = {
            prefix + "SIGNED": arg.shape.signed
            for arg, prefix in zip(args, ["A_", "B_", "S_"])
        }

        return SignalValue(
            rtl.concat(*[
                self.m.operate(celltype, *[s.slot(a.shape.dims[-1], i)
                                           for s, a in zip(arg_signals, args)],
                               Y_WIDTH=retshape.finaldim,
                               **widths, **signednesses)
                for i in range(product(*args[0].shape.dims[:-1]))
            ]),
            retshape,
        )

    @classmethod
    def compose(self, args):
        predims = set(a.shape.dims[:-1] for a in args)
        if len(predims) > 1:
            raise BadInput("can't compose") # TODO

        signed = any(a.shape.signed for a in args)
        lastdim = max(a.shape.signed_finaldim if signed else a.shape.finaldim
                      for a in args)
        common_shape = Shape(*(list(predims)[0] + (lastdim,)), signed=signed)

        return SignalValue(
            rtl.concat(*[a.cast(common_shape).extract_underlying_signal() for a in args]),
            common_shape.prepend_dims((len(args),))
        )

    def on_Op_builtin(self, expr):
        id_    = lambda s: s
        signed = lambda s: s.signed_extend
        mul    = lambda a, b: Shape(a.bitlen + b.bitlen,
                                    signed=a.signed or b.signed)
        max_p1 = lambda a, b: Shape(max(a.bitlen, b.bitlen) + 1,
                                    signed=a.signed or b.signed)
        max_p1_signed \
               = lambda a, b: Shape(max(a.signed_finaldim, b.signed_finaldim) + 1,
                                    signed=True)
        op1    = lambda a, b: Shape(a.bitlen, signed=a.signed)
        p1     = lambda s: Shape(s.bitlen + 1, signed=True)
        const1 = lambda *s: Shape(1, signed=False)

        args = [self(node) for node in expr.args]
        opname = expr.opname

        if opname == "!":
            return self.do_signal_op("$logic_not", const1, *args)
        if opname == "&&":
            return self.do_signal_op("$logic_and", const1, *args)
        if opname == "||":
            return self.do_signal_op("$logic_or", const1, *args)
        if opname == "==":
            return self.do_signal_op("$eq", const1, *args)
        if opname == "!=":
            return self.do_signal_op("$ne", const1, *args)
        if opname == ">=":
            return self.do_signal_op("$ge", const1, *args)
        if opname == ">":
            return self.do_signal_op("$gt", const1, *args)
        if opname == "<=":
            return self.do_signal_op("$le", const1, *args)
        if opname == "<":
            return self.do_signal_op("$lt", const1, *args)
        if opname == "~":
            return self.do_signal_op("$not", signed, *args)
        if opname == "*":
            return self.do_signal_op("$mul", mul, *args)
        if opname == "+":
            return self.do_signal_op("$add", max_p1, *args)
        if opname == "-":
            if len(args) == 2:
                return self.do_signal_op("$sub", max_p1_signed, *args)
            else:
                return self.do_signal_op("$neg", p1, *args)
        if opname == "//":
            return self.do_signal_op("$divfloor", op1, *args)
        if opname == "%":
            return self.do_signal_op("$modfloor", Shape.common, *args)
        if opname == ">>":
            def shape_policy(a, b):
                bitlen = a.finaldim - b.min_value
                if bitlen > 256:
                    print("Warning: Shape of operands of right-shift operation ({a.shape!s} and {b.shape!s}) leads to overly large result (bit length {bitlen})",
                          file=sys.stderr)
                return Shape(bitlen, signed=a.signed)
            if args[1].shape.signed:
                raise NotImplementedError(expr) # TODO
            return self.do_signal_op("$sshr", shape_policy, *args)
        if opname == "<<":
            def shape_policy(a, b):
                bitlen = a.finaldim + b.max_value
                if bitlen > 256:
                    print("Warning: Shape of operands of left-shift operation ({a.shape!s} and {b.shape!s}) leads to overly large result (bit length {bitlen})",
                          file=sys.stderr)
                return Shape(bitlen, signed=a.signed)
            if args[1].shape.signed:
                raise NotImplementedError(expr) # TODO
            return self.do_signal_op("$sshl", shape_policy, *args)
        if opname == "&":
            return self.do_signal_op("$and", Shape.common, *args)
        if opname == "|":
            return self.do_signal_op("$or", Shape.common, *args)
        if opname == "^":
            return self.do_signal_op("$xor", Shape.common, *args)
        if opname == "{":
            return self.compose(args)
        if opname == "?:":
            select = rtl.REDUCE_BOOL(self.m, args[0].extract_signal())
            shape = Shape.common(args[1].shape, args[2].shape)
            return SignalValue(rtl.MUX(self.m,
                                       args[2].cast(shape).extract_underlying_signal(),
                                       args[1].cast(shape).extract_underlying_signal(),
                                       select), shape)

        raise NotImplementedError(opname)

    def on_Op(self, expr):
        if expr.opname not in BASIC_OPS:
            return None

        with expr:
            return self.on_Op_builtin(expr)
