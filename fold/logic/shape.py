# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from ..ast import BadInput
from ..shape import Shape
from ..utils import product
from . import rtl


class Value:
    def __init__(self, shape):
        self.shape = shape

    def extract_signal(self):
        if self.shape.ndims > 1:
            raise BadInput("multi-dimensional value in unsupported expression context")
        return self.extract_underlying_signal()

    def cast(self, *args):
        return SignalValue(self.extract_underlying_signal(), self.shape).cast(*args)


class SignalValue(Value):
    def __init__(self, signal, shape):
        super().__init__(shape)
        assert signal.width == shape.bitlen
        self.signal = signal

    def extract_underlying_signal(self):
        return self.signal

    @classmethod
    def from_const(self, val):
        width = max(val.bit_length(), 1) \
                    + (val < 0)
        return SignalValue(
            rtl.Signal.from_const(val, width),
            Shape(width, signed=val < 0)
        )

    @classmethod
    def _cast_single(self, signal, shape, signed):
        assert shape.ndims == 1
        pad = signal[-1] if signed else rtl.BitState.S0
        return rtl.concat(signal[:shape.bitlen],
                          rtl.Signal.from_bits([pad] * (shape.bitlen - signal.width)))

    def cast(self, othershape):
        if self.shape.dims[:-1] != othershape.dims[:-1]:
            raise BadInput(f"can't cast shape {self.shape!r} to {othershape!r}")

        return SignalValue(
            rtl.concat(*[
                self._cast_single(self.signal.slot(self.shape.dims[-1], i),
                            othershape.drop_dims(othershape.ndims - 1),
                            self.shape.signed)
                for i in range(product(*self.shape.dims[:-1]))
            ]),
            othershape,
        )

    def sub(self, m, *addrs):
        if len(addrs) > self.shape.ndims:
            raise BadInput("bad subscript")
        retshape = self.shape.drop_dims(len(addrs)) \
            if len(addrs) < self.shape.ndims else Shape(1)

        if not retshape.ndims:
            retshape = Shape(1, signed=retshape.signed)

        if isinstance(addrs[0], tuple) and addrs[0][0] == "..":
            # TODO
            assert len(addrs) == 1 and self.shape.ndims == 1
            span = addrs[0][1].signal, addrs[0][2].signal
            assert isinstance(span[0], rtl.Const) \
                    and isinstance(span[1], rtl.Const)
            assert span[1].value <= self.signal.width
            return SignalValue(self.signal[span[0].value:span[1].value + 1],
                               Shape(span[1].value + 1 - span[0].value, signed=self.shape.signed))

        off = rtl.Signal.from_const(0, 1)
        for a, s in zip(addrs, reversed(self.shape.dims)):
            off = rtl.ADD(m,
                rtl.MUL(m, off, rtl.Signal.from_const(s, s.bit_length())),
                a.extract_signal()
            )

        return SignalValue(
            rtl.SHIFTX(m,
                self.signal,
                rtl.MUL(m, rtl.Signal.from_const(retshape.bitlen,
                                retshape.bitlen.bit_length()), off),
                reswidth=retshape.bitlen,
            ),
            retshape,
        )
