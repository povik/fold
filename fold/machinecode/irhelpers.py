# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from contextlib import contextmanager
import llvmlite.ir as ir


def build_aggregate(b, typ, vals):
    built = ir.Constant(typ, ir.Undefined)
    for j, val in enumerate(vals):
        built = b.insert_value(built, val, j)
    return built


def declare_function(b, name, signature):
    m = b.module
    declared = {f.name: f for f in m.functions}
    if name in declared:
        return declared[name]
    return ir.Function(m, signature, name)


def booleanize(b, val):
    if isinstance(val.type, ir.IntType):
        return b.icmp_unsigned("!=", val, ir.Constant(val.type, 0))
    elif isinstance(val.type, ir.ArrayType):
        return build_aggregate(b,
            ir.ArrayType(ir.IntType(1), val.type.count), [
            b.icmp_unsigned("!=",
                b.extract_value(val, i),
                ir.Constant(val.type.element, 0)
            )
            for i in range(val.type.count)
        ])
    else:
        raise NotImplementedError(val.type)


@contextmanager
def malloc(b, typ, size):
    malloc_f = declare_function(b, "malloc",
        ir.FunctionType(ir.PointerType(ir.IntType(8)), (ir.IntType(64),))
    )

    free_f = declare_function(b, "free",
        ir.FunctionType(ir.VoidType(), (ir.PointerType(ir.IntType(8)),))
    )
    # TODO: assumes ir.IntType(64) size
    ptr = b.call(malloc_f, [ir.Constant(ir.IntType(64), size)])
    yield b.bitcast(ptr, ir.PointerType(typ))
    b.call(free_f, [ptr])


def recursive_eq(builder, a, b):
    assert a.type == b.type

    if isinstance(a.type, ir.IntType):
        return builder.icmp_unsigned("==", a, b)
    elif isinstance(a.type, ir.ArrayType):
        ret = ir.Constant(ir.IntType(1), 1)
        for k in range(a.type.count):
            ret = builder.and_(ret,
                recursive_eq(
                    builder,
                    builder.extract_value(a, k),
                    builder.extract_value(b, k),
                )
            )
        return ret
    else:
        raise NotImplementedError(a.type)
