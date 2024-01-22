# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from functools import reduce
from .ast import Tuple, BadInput


def pow2ceil(a):
    b = (a - 1)
    b = b | b >> 1
    b = b | b >> 2
    b = b | b >> 4
    b = b | b >> 8
    b = b | b >> 16
    b = b | b >> 32
    return b + 1


def log2ceil(a):
    return (a-1).bit_length()


def sign_extend(value, bits):
    sign_bit = 1 << (bits - 1)
    return (value & (sign_bit - 1)) - (value & sign_bit)


class Hints:
    def __init__(self, text):
        self.text = text

    def __getitem__(self, key):
        kvs = (t.strip().split(" ", 2) for t in self.text.split("."))
        matches = [(kv[1] if len(kv)==2 else True) for kv in kvs if kv[0] == key]
        return matches[0] if len(matches)>0 else None


def hint(*name, immediate=False):
    for m in [Tuple.curr_markers] + ([] if immediate else Tuple.markers_stack):
        if m is not None and m[1] is not None:
            hints = Hints(m[1].hint_text)
            for n in name:
                if hints[n]:
                    return hints[n]
    return None


def hint_pre(*name, immediate=False):
    for m in [Tuple.curr_markers] + ([] if immediate else Tuple.markers_stack):
        if m is not None and m[0] is not None:
            hints = Hints(m[0].hint_text)
            for n in name:
                if hints[n]:
                    return hints[n]
    return None


def mrange(*args, prepend=()):
    if not len(args):
        yield prepend
    else:
        for i in range(args[0]):
            yield from mrange(*args[1:], prepend=prepend + (i,))


def encode_multiindex(js, dims):
    ret = 0
    for j, l in zip(js, dims):
        ret = ret * l + j
    return ret


def product(*args):
    return reduce(lambda a, b: a * b, args, 1)
