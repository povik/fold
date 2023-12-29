# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from . import rtl


def execid_operate(m, opname, *args, result_width=None, **params):
    retwire = m.add_wire("$execid_result", result_width)
    m.add_cell(opname, ("\\Y", retwire),
               *zip(["\\EN", "\\A", "\\B"], args),
               **params)
    return retwire

def ZERO(m, en, width=None):
    assert width is not None
    return execid_operate(m, "\\EXECID_ZERO", en,
                          result_width=width, WIDTH=width)

def INC(m, a, en):
    return execid_operate(m, "\\EXECID_INC", en, a,
                          result_width=a.width, WIDTH=a.width)

def DESCEND(m, a, en):
    return execid_operate(m, "\\EXECID_DESCEND", en, a,
                          result_width=a.width, WIDTH=a.width)

def ASCEND(m, a, en):
    return execid_operate(m, "\\EXECID_ASCEND", en, a,
                          result_width=a.width, WIDTH=a.width)

def FORK(m, a, en):
    ret = m.add_wire("$execid_result", a.width)
    m.add_cell("\\EXECID_FORK", ("\\Y", ret),
               ("\\EN", en), ("\\A", a), WIDTH=a.width)
    return ret

def GT(m, a, b, en):
    assert a.width == b.width
    return execid_operate(m, "\\EXECID_GT", en, a, b,
                          result_width=1, WIDTH=a.width)

def GE(m, a, b, en):
    assert a.width == b.width
    return execid_operate(m, "\\EXECID_GE", en, a, b,
                          result_width=1, WIDTH=a.width)

def EQ(m, a, b, en):
    assert a.width == b.width
    return execid_operate(m, "\\EXECID_EQ", en, a, b,
                          result_width=1, WIDTH=a.width)

def MAX(m, cases, y_width=None):
    assert y_width is not None
    assert len(cases) > 0

    for en, val in cases:
        assert en.width == 1
        assert val.width == y_width

    return (execid_operate(m, "\\EXECID_MAX", rtl.concat(*[en for en, _ in cases]),
                          rtl.concat(*[val for _, val in cases]),
                          result_width=y_width, Y_WIDTH=y_width, WIDTH=len(cases) * y_width),
            rtl.OR(m, *[en for en, _ in cases]))
