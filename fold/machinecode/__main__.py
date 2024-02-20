# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import os.path
import io
import sys
import signal
import argparse
import pathlib
import llvmlite.ir as ir
import llvmlite.ir.builder
import llvmlite.binding as llvm
from functools import cache, reduce
from contextlib import contextmanager
import contextlib

from .. import ast, utils
from .. import eval as baseeval
from ..shape import Shape
from ..utils import log2ceil

from . import irhelpers


@llvmlite.ir.builder._unop('freeze')
def llvm_freeze(builder, value, name='', flags=()):
    '''
    Builder function to emit "freeze" instructions in LLVM IR (this is a workaround
    for missing a method on llvmlite `Builder` class)
    '''
    pass


def ir_type(shape):
    ret = ir.IntType(shape.finaldim)
    for dim in reversed(shape.dims[:-1]):
        ret = ir.ArrayType(ret, dim)
    return ret


def _to_llvm_typeref(typ):
    '''Convert a ir.Type into llvm.TypeRef'''
    # Due to limitations of llvmlite we need to go through
    # declaring a global variable in a throwaway module, then
    # letting LLVM parse the module
    m = ir.Module()
    glob = ir.GlobalVariable(m, typ, "canary")
    m_llvm = llvm.parse_assembly(str(m))
    typ_llvm = m_llvm.get_global_variable("canary").type.element_type
    return typ_llvm


class Value:
    def __init__(self, shape, irval, irptr=None):
        assert isinstance(irval, ir.Value)
        assert isinstance(shape, Shape)
        assert irval.type == ir_type(shape)
        self.shape = shape
        self.ir = irval
        self.irptr = irptr

    @property
    def have_ptr(self):
        return self.irptr is not None

    @classmethod
    def from_const(self, shape, const):
        return Value(shape, ir.Constant(ir_type(shape), const))

    @classmethod
    def _cast(self, b, shape, othershape, irval):
        if shape.dims[:-1] != othershape.dims[:-1]:
            raise ast.BadInput(f"can't cast shape {shape!r} to {othershape!r}")

        if othershape.ndims != 1 or shape.ndims != 1:
            cast_components = [
                self._cast(b,
                    shape.drop_dims(1),
                    othershape.drop_dims(1),
                    b.extract_value(irval, j)
                )
                for j in range(shape[0])
            ]
            return irhelpers.build_aggregate(b,
                ir_type(othershape),
                cast_components
            )

        if othershape.bitlen < shape.bitlen:
            cast_ir = b.trunc(irval, ir_type(othershape))
        elif othershape.bitlen > shape.bitlen and shape.signed:
            cast_ir = b.sext(irval, ir_type(othershape))
        elif othershape.bitlen > shape.bitlen and not shape.signed:
            cast_ir = b.zext(irval, ir_type(othershape))
        else:
            assert othershape.bitlen == shape.bitlen
            cast_ir = irval

        return cast_ir

    def recast(self, b, othershape):
        if othershape == self.shape:
            return self
        cast_ir = self._cast(b, self.shape, othershape, self.ir)
        return Value(othershape, cast_ir)

    @contextmanager
    def ir_ptr(self, b, target_data):
        if self.have_ptr:
            yield self.irptr
        else:
            size = target_data.get_abi_size(_to_llvm_typeref(self.ir.type))
            with irhelpers.malloc(b, self.ir.type, size) as scratch:
                b.store(self.ir, scratch)
                yield scratch
                b.store(ir.Constant(self.ir.type, ir.Undefined), scratch)


Value.VOID = Value.from_const(Shape(0), 0)

BUILTIN_OPS = dict()
def register_builtin_op(name):
    def _register(f):
        BUILTIN_OPS[name] = f
    return _register


def compose_value(b, args):
    predims = set(a.shape.dims[:-1] for a in args)
    if len(predims) > 1:
        shapes_str = ", ".join(str(a.shape) for a in args)
        raise ast.BadInput(f"bad shapes for composition: {shapes_str}")

    element_shape = reduce(Shape.common, [a.shape.element for a in args])
    component_shape = element_shape.prepend_dims(list(predims)[0])
    retshape = component_shape.prepend_dims((len(args),))
    irval = irhelpers.build_aggregate(
        b, ir_type(retshape),
        [a.recast(b, component_shape).ir for a in args]
    )
    return Value(retshape, irval)


def recurse_on_highdims(f):
    def wrapper(b, operands, **kwargs):
        if len(set(a.shape.dims[:-1] for a in operands)) != 1:
            shapes_str = ", ".join(str(a.shape) for a in operands)
            raise ast.BadInput(f"bad shapes of operands: {shapes_str}")

        # Recurse on ndims > 1
        if operands[0].shape.ndims > 1:
            return compose_value(b, [
                wrapper(b, *[
                    Value(a.shape.drop_dims(1), b.extract_value(a.ir, j))
                    for a in operands
                ], **kwargs)
                for j in range(operands[0].shape.dims[0])
            ])

        return f(b, operands, **kwargs)
    return wrapper


def check_nargs(args, expected):
    if type(expected) is int:
        if len(args) != expected:
            raise ast.BadInput(f"operation requires {expected} arguments")
        return
    elif len(args) not in expected:
        raise ast.BadInput("bad number of arguments to the operation")


global_counter = 0
@register_builtin_op("print")
def builtin_print(b, argvals):
    global global_counter
    printf_f = irhelpers.declare_function(b, "printf",
        ir.FunctionType(ir.IntType(32), [ir.PointerType(ir.IntType(8))], var_arg=True)
    )
    str_ir = argvals[0].ir
    global_counter += 1
    glob = ir.GlobalVariable(b.module, str_ir.type, "xxx%d" % global_counter) # TODO
    glob.global_constant = True
    glob.initializer = str_ir
    b.call(printf_f, [b.bitcast(glob, ir.PointerType(ir.IntType(8)))] + [
        arg.recast(b, Shape(64, signed=arg.shape.signed)).ir for arg in argvals[1:]
    ])
    return Value.VOID


@register_builtin_op("read_tsv!")
def read_tsv(b, argvals):
    check_nargs(argvals, 1)
    filename = bytes(argvals[0].ir.constant[:-1])
    data = []
    try:
        with open(filename, "r") as f:
            for line in f:
                data.append([int(m) for m in line.split()])
    except FileNotFoundError:
        raise ast.BadInput("file not found: {:h}", filename.decode("ascii"))

    shape = Shape(len(data), len(data[0]), 32, signed=True)
    gconst = ir.GlobalVariable(b.module, ir_type(shape), "yyy%d" % ast.Tuple.curr_markers[0].line)
    gconst.global_constant = True
    gconst.initializer = ir.Constant(ir_type(shape), data)
    return Value(shape, b.load(gconst))


@register_builtin_op("ctz")
@recurse_on_highdims
def builtin_ctz(b, argvals):
    check_nargs(argvals, 1)
    retshape = Shape(argvals[0])
    return Value(Shape(argvals[0].shape.bitlen),
        b.cttz(argvals[0].ir, ir.Constant(ir.IntType(1), 1))
    ) # TODO: shape


@register_builtin_op("r")
@recurse_on_highdims
def builtin_ctz(b, argvals):
    check_nargs(argvals, 1)
    return argvals[0]


@register_builtin_op("__abort")
def builtin_abort(b, argvals):
    check_nargs(argvals, 0)
    abort_f = irhelpers.declare_function(b, "abort",
        ir.FunctionType(ir.VoidType(), ())
    )
    b.call(abort_f, [])
    return Value.VOID


@register_builtin_op("__fork")
def builtin_fork(b, argvals):
    check_nargs(argvals, 0)
    fork_f = irhelpers.declare_function(b, "fork",
        ir.FunctionType(ir.IntType(32), ())
    )
    return Value(Shape(32), b.call(fork_f, []))


@register_builtin_op("assert_equal")
def builtin_assert_equal(b, argvals):
    check_nargs(argvals, 2)
    arg1, arg2 = argvals
    common_type = Shape.common(arg1.shape, arg2.shape)

    eq = irhelpers.recursive_eq(b,
        llvm_freeze(b, arg1.recast(b, common_type).ir),
        llvm_freeze(b, arg2.recast(b, common_type).ir)
    )

    abort_f = irhelpers.declare_function(b, "abort",
        ir.FunctionType(ir.VoidType(), ())
    )
    with b.if_then(b.not_(eq)):
        b.call(abort_f, [])

    return Value.VOID


@register_builtin_op("cover")
def builtin_assert_equal(b, argvals):
    check_nargs(argvals, [0, 1])
    # TODO
    return Value.VOID


# here, ExprEvaluator gets a special version of the decorator
def recurse_on_highdims2(f):
    def wrapper(self, arg1, arg2, *operands, **kwargs):
        if len(set(a.shape.dims[:-1] for a in operands)) != 1:
            shapes_str = ", ".join(str(a.shape) for a in operands)
            raise ast.BadInput(f"bad shapes of operands: {shapes_str}")

        # Recurse on ndims > 1
        if operands[0].shape.ndims > 1:
            return self.compose([
                wrapper(self, arg1, arg2, *[
                    Value(a.shape.drop_dims(1), self.b.extract_value(a.ir, j))
                    for a in operands
                ], **kwargs)
                for j in range(operands[0].shape.dims[0])
            ])

        return f(self, arg1, arg2, *operands, **kwargs)
    return wrapper


# TODO: check `common_shape` variables versus shape policies
# does the result IR shape match up only by accident?
class ExprEvaluator(baseeval._ExprEvaluator):
    def __init__(self, frame):
        self.frame = frame

    @property
    def b(self):
        return self.frame.builder

    def _val_from_constval(self, constval):
        if type(constval) is str:
            encoded = bytearray(constval.encode("ascii")[1:-1] + b"\0") # TODO
            shape = Shape(len(encoded), 8, signed=False)
            return Value.from_const(shape, encoded)
        elif type(constval) is int:
            signed = constval < 0
            width = max(constval.bit_length(), 1) \
                    + (1 if signed else 0)
            return Value.from_const(Shape(width, signed=signed),
                                    constval)
        else:
            raise ValueError(expr.val)

    def on_Const(self, expr):
        return self._val_from_constval(expr.val)

    def on_Var(self, expr):
        if expr.varname == "undef":
            return Value.from_const(Shape(1), 0)

        if expr.varname in self.frame.d._consts:
            val = self.frame.d._consts[expr.varname]
            return self._val_from_constval(val)

        try:
            _, var = self.frame.find_var(expr.varname)
        except KeyError:
            raise ast.BadInput("no such variable: {:h}", expr.varname)

        return self.frame.lookup_var_state(var)

    def compose(self, args):
        predims = set(a.shape.dims[:-1] for a in args)
        if len(predims) > 1:
            shapes_str = ", ".join(str(a.shape) for a in args)
            raise ast.BadInput("bad shapes for composition: {shapes_str}")

        element_shape = reduce(Shape.common, [a.shape.element for a in args])
        component_shape = element_shape.prepend_dims(list(predims)[0])
        retshape = component_shape.prepend_dims((len(args),))
        irval = irhelpers.build_aggregate(
            self.b, ir_type(retshape),
            [a.recast(self.b, component_shape).ir for a in args]
        )
        return Value(retshape, irval)

    @recurse_on_highdims2
    def do_arith_op(self, opname, shape_policy, *operands):
        assert len(operands) <= 2
        assert all(a.shape.is_single_dim for a in operands)
        retshape = shape_policy(*[val.shape.element for val in operands])
        irval = getattr(self.b, opname)(*[
            arg.recast(self.b, retshape).ir for arg in operands
        ])
        return Value(retshape, irval)

    @recurse_on_highdims2
    def do_bi_op(self, opname, shape_policy, a, b, opargs=[]):
        assert a.shape.is_single_dim and b.shape.is_single_dim
        retshape = shape_policy(a, b)
        common_shape = Shape.common(a.shape, b.shape)
        irval = getattr(self.b, opname)(*opargs,
            *[arg.recast(self.b, common_shape).ir for arg in (a, b)]
        )
        return Value(retshape, irval)

    def do_cmp_op(self, opname, a, b, opargs=[]):
        if a.shape.ndims > 1 or b.shape.ndims > 1:
            raise NotImplementedError((a.shape, b.shape))

        common_shape = Shape.common_signed(a.shape, b.shape)
        irval = self.b.icmp_signed(opname,
            *[arg.recast(self.b, common_shape).ir for arg in (a, b)]
        )
        return Value(Shape(1), irval)

    @classmethod
    def _repeat(self, element, dims):
        if len(dims) == 0:
            return element
        return [self._repeat(element, dims[1:])] * dims[0]

    def on_Op_elementary(self, expr, args):
        id_    = lambda s: s
        signed = lambda s: s.signed_extend
        mul    = lambda a, b: Shape(a.bitlen + b.bitlen,        signed=a.signed or b.signed)
        max_   = lambda a, b: Shape(max(a.bitlen, b.bitlen),    signed=a.signed or b.signed)
        max_p1 = lambda a, b: Shape(max(a.bitlen, b.bitlen) + 1,signed=a.signed or b.signed)
        max_signed \
               = lambda a, b: Shape(max(a.signed_finaldim, b.signed_finaldim), signed=True)
        op1    = lambda a, b: Shape(a.bitlen, signed=a.signed)
        op1_signed = lambda a, b: Shape(a.signed_finaldim, signed=True)
        const1 = lambda *s: Shape(1, signed=False)

        opname = expr.opname

        if opname == "{":
            return self.compose(args)
        if opname == "!":
            return self.do_bi_op("icmp_unsigned", const1, args[0],
                Value.from_const(Shape(1).prepend_dims(args[0].shape.dims[:-1]),
                                 self._repeat(0, args[0].shape.dims[:-1])),
            opargs=["=="])
        if opname in ["&&", "||"]:
            if args[0].shape.arraydims != args[1].shape.arraydims:
                raise BadInput("irreconcilable dimensions of operands: {:h} and {:h}",
                               args[0].shape, args[1].shape)
            arraydims = args[0].shape.arraydims

            op1 = irhelpers.booleanize(self.b, args[0].ir)
            op2 = irhelpers.booleanize(self.b, args[1].ir)
            if opname == "&&":
                ret_irval = self.b.and_(op1, op2)
            elif opname == "||":
                ret_irval = self.b.or_(op1, op2)
            else:
                raise NotImplementedError(opname)

            return Value(Shape(1).prepend_dims(arraydims), ret_irval)
        if opname == "==":
            return self.do_bi_op("icmp_unsigned", const1, *args, opargs=["=="])
        if opname == "!=":
            return self.do_bi_op("icmp_unsigned", const1, *args, opargs=["!="])
        if opname == "~":
            return self.do_arith_op("not_", signed, *args)
        if opname == "*":
            return self.do_arith_op("mul", mul, *args)
        if opname == "+":
            return self.do_arith_op("add", max_p1, *args)
        if opname == "-":
            def shape_policy(a, b):
                return Shape(max(a.signed_finaldim, b.signed_finaldim) + 1,
                             signed=True)

            if len(args) == 2:
                return self.do_arith_op("sub", shape_policy, *args)
            else:
                return self.do_arith_op("sub", shape_policy,
                            Value.from_const(Shape(1), 0), args[0])
        if opname == "<":
            return self.do_cmp_op("<", *args)
        if opname == ">":
            return self.do_cmp_op(">", *args)
        if opname == "<=":
            return self.do_cmp_op("<=", *args)
        if opname == ">=":
            return self.do_cmp_op(">=", *args)
        if opname == "//":
            helper_ast = ast.parse_expr_from_buffer('''
                (b >= 0) ? (a >= 0 ? __sdiv(a, b) : __sdiv(a + 1, b) - 1)
                         : (-a >= 0 ? __sdiv(-a, -b) : __sdiv(-a + 1, -b) - 1)
            ''', "compiler internal")
            new_ast = helper_ast.replace({ast.Var('a'): ast.Special(args[0]),
                                          ast.Var('b'): ast.Special(args[1])})
            return self(new_ast)
        if opname == "__sdiv":
            return self.do_arith_op("sdiv", max_signed, *args)
        if opname == "%":
            helper_ast = ast.parse_expr_from_buffer('''
                (b >= 0) ? (a >= 0 ? __srem(a, b) : b - 1 + __srem(a + 1, b))
                         : -(-a >= 0 ? __srem(-a, -b) : -b - 1 + __srem(-a + 1, -b))
            ''', "compiler internal")
            new_ast = helper_ast.replace({ast.Var('a'): ast.Special(args[0]),
                                          ast.Var('b'): ast.Special(args[1])})
            return self(new_ast)
        if opname == "__srem":
            return self.do_arith_op("srem", max_signed, *args)
        if opname == ">>":
            def shape_policy(a, b):
                bitlen = a.finaldim - b.min_value
                if bitlen > 256:
                    print(f"Warning: Shape of operands of right-shift operation ({a!s} and {b!s}) leads to overly large result (bit length {bitlen})",
                          file=sys.stderr)
                return Shape(bitlen, signed=a.signed)
            if args[0].shape.signed:
                return self.do_arith_op("ashr", shape_policy, *args)
            else:
                return self.do_arith_op("lshr", shape_policy, *args)
        if opname == "<<":
            def shape_policy(a, b):
                bitlen = a.finaldim + b.max_value
                if bitlen > 256:
                    print(f"Warning: Shape of operands of left-shift operation ({a!s} and {b!s}) leads to overly large result (bit length {bitlen}",
                          file=sys.stderr)
                return Shape(bitlen, signed=a.signed)

            return self.do_arith_op("shl", shape_policy, *args)
        if opname == "&":
            return self.do_arith_op("and_", Shape.common, *args)
        if opname == "^":
            return self.do_arith_op("xor", Shape.common, *args)
        if opname == "|":
            return self.do_arith_op("or_", Shape.common, *args)
        if opname == "?:":
            cond = args[0]

            if cond.shape.ndims != 1:
                raise ast.BadInput("bad shape of first operand to ternary operator: {:h}", cond.shape)

            cond_ir = irhelpers.booleanize(self.b, cond.ir)
            common_shape = Shape.common(args[1].shape, args[2].shape)
            irval = self.b.select(cond_ir,
                args[1].recast(self.b, common_shape).ir,
                args[2].recast(self.b, common_shape).ir
            )
            return Value(common_shape, irval)

        return None

    def on_indexing(self, expr, argvals):
        base_val = argvals[0]
        indices = argvals[1:]
        with base_val.ir_ptr(self.b, self.frame.d.target_data) as ptr:
            # TODO: out of bounds checking
            # The first index to gep is always zero (to dereference the pointer as-is)
            gep_indices = [ir.Constant(ir.IntType(1), 0)]
            # Take off all user indices but the one corresponding to the final dimension
            nindices_done = 0
            for i, arg in enumerate(indices[:base_val.shape.ndims - 1]):
                if type(arg) is not Value:
                    with expr.args[1 + i]:
                        raise ast.BadInput("bit span syntax a..b not supported on other than final dimension")
                gep_indices.append(arg.recast(self.b, arg.shape.signed_extend).ir)
                nindices_done += 1
            finaldim_val = Value(
                base_val.shape.drop_dims(nindices_done),
                self.b.load(self.b.gep(ptr, gep_indices))
            )
            if nindices_done == len(indices):
                return finaldim_val
            else:
                if type(indices[-1]) is tuple:
                    kind = indices[-1][0]
                    final_index = indices[-1][1:]
                    if not all(type(v.ir) is ir.Constant for v in final_index):
                        with expr.args[-1]:
                            raise ast.BadInput("bit span indices must be constant")
                    a, b = final_index
                    an, bn = a.ir.constant, b.ir.constant + (1 if kind == ".." else 0)
                    if an >= bn or not all(v in range(0, base_val.shape.finaldim + 1) for v in (an, bn)):
                        with expr.args[-1]:
                            raise ast.BadInput("bit span indices out of bounds")
                    shamount = self._val_from_constval(an)
                    common_shape = Shape.common(finaldim_val.shape, shamount.shape)
                    lsh_irval = self.b.lshr(
                        finaldim_val.recast(self.b, common_shape).ir,
                        shamount.recast(self.b, common_shape).ir
                    )
                    retshape = Shape(bn - an)
                    return Value(retshape, self.b.trunc(lsh_irval, ir_type(retshape)))
                else:
                    final_index = indices[-1]
                    assert type(final_index) is Value
                    common_shape = Shape.common(finaldim_val.shape, final_index.shape)
                    lsh_irval = self.b.lshr(
                        finaldim_val.recast(self.b, common_shape).ir,
                        final_index.recast(self.b, common_shape).ir
                    )
                    return Value(Shape(1), self.b.trunc(lsh_irval, ir.IntType(1)))

    def on_Op(self, expr):
        opname = expr.opname
        argvals = [self(node) for node in expr.args]

        val = self.on_Op_elementary(expr, argvals)
        if val is not None:
            return val

        if expr.opname == ":":
            return (":", *argvals)

        if expr.opname == "..":
            return ("..", *argvals)

        if opname == "[":
            return self.on_indexing(expr, argvals)

        if opname in BUILTIN_OPS:
            return BUILTIN_OPS[opname](self.b, argvals)

        func = self.frame.d.request_func(expr.opname)

        if len(func.rets) != 1:
            raise ast.BadInput("function call in expression context is required to return a single value ({:h} returns {:h} values)",
                           expr.opname, len(retsdecl))

        ret_irtype = ir_type(func.rets[0].shape)
        size = self.frame.d.target_data.get_abi_size(_to_llvm_typeref(ret_irtype))
        with irhelpers.malloc(self.b, ret_irtype, size) as scratch:
            impl_callsite(self.frame, func, argvals, [scratch])
            return Value(func.rets[0].shape, self.b.load(scratch))

    def on_Special(self, expr):
        return expr.val


class VarImpl:
    def __init__(self, frame, varname, shape, read=True, write=True):
        self.f = frame
        self.varname = varname
        self.shape = shape
        self.read = read
        self.write = write


BASIC_OPS = ["~", "*", "+", "-", "<<", ">>",
             "&", "|", "==", "!=", "<", "<=",
             ">", ">=", "//", "%", "{", "!",
             "&&", "||", "[", "?:", "^"]


def impl_callsite(frame, func, argvals, retptrs):
    b = frame.b

    with contextlib.ExitStack() as stack:
        ir_args = []

        if len(argvals) != len(func.args):
            raise ast.BadInput("bad number of arguments")

        for argval, arg in zip(argvals, func.args):
            ir_args.append(
                stack.enter_context(argval.recast(b, arg.shape) \
                                    .ir_ptr(b, frame.d.target_data))
            )

        for var in func.vars_closure:
            # r/w permissions are checked when the function body is implemented
            ir_args.append(stack.enter_context(frame.var_ptr(var)))

        assert len(retptrs) == len(func.rets)
        ir_args += retptrs

        frame.b.call(func.ir_function, ir_args)


class Frame:
    def __init__(self, d, varptrs, parent_frame=None):
        self.d = d
        self.parent_frame = parent_frame
        self.vars = dict()
        self.labels = dict()
        self.of_function = None
        self.varptrs = dict(varptrs)
        self.builder = parent_frame.builder if parent_frame is not None else None

    @property
    def b(self):
        return self.builder

    @property
    def function_parent(self):
        for f in self.stack:
            if f.of_function is not None:
                return f.of_function
        assert False

    @property
    def stack(self):
        f = self
        while f is not None:
            yield f
            f = f.parent_frame

    def find_var(self, varname):
        for f in self.stack:
            if varname in f.vars:
                return f, f.vars[varname]

        raise KeyError(varname)

    def lookup_var_state(self, var):
        if not var.read:
            raise ast.BadInput("variable {:h} cannot be read here", var.varname)
        return Value(var.shape, self.builder.load(self.varptrs[var]), self.varptrs[var])

    @contextmanager
    def var_ptr(self, var, write=False, read=False):
        if write and not var.write:
            raise ast.BadInput("variable {:h} cannot be written to here", var.varname)
        if read and not var.read:
            raise ast.BadInput("variable {:h} cannot be read here", var.varname)
        yield self.varptrs[var]

    @property
    def vars_recursive(self):
        return list(self.vars.values()) + list(self.parent_frame.vars_recursive \
                                          if self.parent_frame is not None else [])

    @classmethod
    def from_ast_body(self, d, body, varptrs, parent_frame=None, inject_labels={}):
        frame = Frame(d, varptrs, parent_frame=parent_frame)
        frame.labels.update(inject_labels)
        return frame.impl_ast_body(body)

    def _lookup_label_frame(self, label, within_function=True):
        frame = self
        while frame is not None \
                and label not in frame.labels \
                and (frame.of_function is None or not within_function):
            frame = frame.parent_frame
        if label not in frame.labels:
            return None
        return frame

    def lookup_label(self, label):
        frame = self._lookup_label_frame(label)

        if frame is None:
            if self._lookup_label_frame(label, within_function=False) is not None:
                raise ast.BadInput("jumping out of functions not supported")
            if label == "%break":
                raise ast.BadInput("nowhere to break from")
            raise ast.BadInput("no such label: {:h}", label)
        return frame.labels[label]

    def impl_assignment(self, lhs, val):
        addrs = []

        if isinstance(lhs, ast.Op):
            with lhs:
                if lhs.opname != "[":
                    raise ast.BadInput("bad operation on left-hand side of assignment")
                addrs = [self.eval(arg) for arg in lhs.args[1:]]
                lhs = lhs.args[0]
        with lhs:
            if not isinstance(lhs, ast.Var):
                raise ast.BadInput("bad expression on left-hand side of assignment")
        try:
            frame, var = self.find_var(lhs.varname)
        except KeyError:
            raise ast.BadInput("no such variable: {:h}", lhs.varname)

        with self.var_ptr(var, write=True) as ptr:
            gep_indices = [ir.Constant(ir.IntType(1), 0)] + [
                a.recast(self.b, a.shape.signed_extend).ir for a in addrs
            ]
            ptr_subbed = self.b.gep(ptr, gep_indices)
            self.b.store(val.recast(self.b, var.shape.drop_dims(len(addrs))).ir, ptr_subbed)

    def impl_var(self, vardecl):
        with vardecl as (varname, shapenode):
            if varname in self.vars:
                raise ast.BadInput("duplicate variable '{:h}' in this scope", varname)
            shape = self.d.eval_shape(shapenode)
            self.vars[varname] = var = VarImpl(self, varname, shape)

        with self.builder.goto_block(self.function_parent.alloc_block):
            self.varptrs[var] = self.b.alloca(ir_type(var.shape))
            self.varptrs[var].name = varname

    @property
    @cache
    def eval(self):
        return ExprEvaluator(self)

    def impl_top_expr(self, lhslist, rhs):
        if type(rhs) is ast.Op and rhs.opname == "delay":
            return None
        if type(rhs) is ast.Op and (rhs.opname not in BASIC_OPS) and rhs.opname not in BUILTIN_OPS:
            opname = rhs.opname
            argvals = [self.eval(node) for node in rhs.args]
            func = self.d.request_func(rhs.opname)

            with contextlib.ExitStack() as stack:
                retptrs = []
                for ret in func.rets:
                    irtype = ir_type(ret.shape)
                    size = self.d.target_data.get_abi_size(_to_llvm_typeref(irtype))
                    retptrs.append(stack.enter_context(irhelpers.malloc(self.b, irtype, size)))

                impl_callsite(self, func, argvals, retptrs)

                retvals = [
                    Value(var.shape, self.b.load(retptr))
                    for var, retptr in zip(func.rets, retptrs)
                ]

            if len(func.rets) != len(lhslist):
                raise ast.BadInput("assignment mismatch, function {:h} returns {} values, given {} sites",
                                   rhs.opname, len(func.rets), len(lhslist))

            for no, lhs_retval in enumerate(zip(lhslist, retvals)):
                lhs, retval = lhs_retval
                if isinstance(lhs, ast.Var) and lhs.varname == "_":
                    continue
                self.impl_assignment(lhs, retval)
        else:
            if len(lhslist) > 1:
                raise ast.BadInput("multiple value assignment of a single value expression result")
            res = self.eval(rhs)
            if len(lhslist) > 0:
                self.impl_assignment(lhslist[0], res)

    def impl_ast_body(self, ast_body):
        builder = self.builder

        for stat in ast_body:
            if stat[0] == "var":
                with stat as (_, varlist):
                    for vardecl in varlist:
                        self.impl_var(vardecl)

        for stat in ast_body:
            if stat[0] == "label":
                with stat as (_, label):
                    self.labels[label] = builder.append_basic_block(label)

        for stat in ast_body:
            if stat[0] == "label":
                with stat as (_, label):
                    block = self.labels[label]
                    builder.branch(block)
                    builder.position_at_end(block)

            if stat[0] == "=":
                with stat as (_, lhslist, rhslist):
                    if len(rhslist) == 1:
                        self.impl_top_expr(lhslist, rhslist[0])
                    elif len(lhslist) != len(rhslist):
                        raise ast.BadInput("LHS/RHS mismatch")
                    else:
                        for lhs, rhs in zip(lhslist, rhslist):
                            self.impl_top_expr([lhs], rhs)
            elif isinstance(stat[0], list):
                with stat as (rhslist,):
                    for rhs in rhslist:
                        self.impl_top_expr([], rhs)
            elif stat[0] == "if":
                with stat as (_, cond, truebody, falsebody):
                    condval = ExprEvaluator(self)(cond)

                    if condval.shape.ndims > 1:
                        raise ast.BadInput("condition value needs to be single-dimensional (has shape {:h} instead)",
                                           condval.shape)

                    with self.b.if_else(irhelpers.booleanize(self.b, condval.ir)) \
                            as (then, otherwise):
                        with then:
                            Frame.from_ast_body(self.d, truebody, self.varptrs,
                                                parent_frame=self)
                        with otherwise:
                            Frame.from_ast_body(self.d, falsebody, self.varptrs,
                                                parent_frame=self)

            elif stat[0] == "func":
                self.d._function_asts[stat[1]] = (self, stat)
            elif stat[0] == "goto":
                with stat as (_, label):
                    block = self.lookup_label(label)
                    builder.branch(block)
                    scratch = builder.append_basic_block()
                    builder.position_at_end(scratch)
            elif stat[0] == "for":
                with stat as (_, cond, body):
                    pred_block = builder.append_basic_block()
                    body_block = builder.append_basic_block()
                    exit_block = builder.append_basic_block()

                    builder.branch(pred_block)

                    with builder.goto_block(pred_block):
                        condval = ExprEvaluator(self)(cond)
                        if condval.shape.ndims > 1:
                            raise ast.BadInput("condition value needs to be single-dimensional (has shape {:h} instead)",
                                               condval.shape)

                        with builder.if_then(irhelpers.booleanize(self.b, condval.ir)):
                            builder.branch(body_block)
                        builder.branch(exit_block)

                    with builder.goto_block(body_block):
                        frame = Frame.from_ast_body(
                            self.d, body, self.varptrs,
                            parent_frame=self,
                            inject_labels={"%break": exit_block},
                        )
                        builder.branch(pred_block)

                    builder.position_at_end(exit_block)
            elif stat[0] == "{":
                with stat as (_, body):
                    Frame.from_ast_body(self.d, body, self.varptrs,
                                        parent_frame=self)
            elif stat[0] == "const":
                pass
            elif stat[0] == "var":
                pass
            elif stat[0] == "label":
                pass
            elif stat[0] == "fork":
                with stat as (_, label):
                    block = self.lookup_label(label)
                    if utils.hint_pre("mcode_fork") == "take_jump":
                        builder.branch(block)
                        scratch = builder.append_basic_block()
                        builder.position_at_end(scratch)
                    elif utils.hint_pre("mcode_fork") == "ignore_jump":
                        continue
                    else:
                        if self.d.forks_disabled:
                            raise ast.BadInput("forks disabled")
                        helper_ast = ast.parse_spec_from_buffer('''
                            var pid [32] mut;
                            pid = __fork();
                            if pid < 0 {
                                print("failed fork\n");
                                __abort();
                            }
                            if pid {
                                goto thread2;
                            }
                        ''', "compiler internal")
                        Frame.from_ast_body(self.d, helper_ast, {},
                                            parent_frame=self,
                                            inject_labels={"thread2": block})
            elif stat[0] == "quit":
                with stat as (_,):
                    exit_f = irhelpers.declare_function(builder, "exit",
                        ir.FunctionType(ir.VoidType(), (ir.IntType(32),)) # TODO
                    )
                    builder.call(exit_f, [ir.Constant(ir.IntType(32), 0)])
            else:
                raise NotImplementedError(stat)


class Function:
    def __init__(self, d, frame):
        self.d = d
        self.frame = frame
        self.args = []
        self.rets = []
        self.signature_finalized = False

    @property
    @cache
    def vars_closure(self):
        return list(self.frame.parent_frame.vars_recursive) \
               if (self.frame.parent_frame is not None) else []

    @property
    def llvm_signature(self):
        assert self.signature_finalized
        return ir.FunctionType(
            ir.VoidType(),
            [
                ir.PointerType(ir_type(var.shape))
                for var in self.args + self.vars_closure + self.rets
            ]
        )

    @classmethod
    def from_ast(self, d, node, parent_frame):
        with node as (_1, name, argsdecl, retsdecl, body):
            frame = Frame(d, dict(), parent_frame)
            func = Function(d, frame)
            frame.of_function = func

            for argdecl in argsdecl:
                with argdecl as (varname, shapenode):
                    shape = d.eval_shape(shapenode)
                    var = VarImpl(self, varname, shape, write=False)
                    frame.vars[varname] = var
                    func.args.append(var)
            for retdecl in retsdecl:
                with retdecl as (varname, shapenode):
                    shape = d.eval_shape(shapenode)
                    var = VarImpl(self, varname, shape, read=False)
                    frame.vars[varname] = var
                    func.rets.append(var)
            func.signature_finalized = True

            func.ir_function = ir.Function(d.ir_module, func.llvm_signature, name=name)
            if parent_frame is not None:
                func.ir_function.linkage = "private"
            func.alloc_block = func.ir_function.append_basic_block(name="alloc")
            block = func.ir_function.append_basic_block(name="entry")
            frame.builder = builder = ir.IRBuilder(block)
            with builder.goto_block(func.alloc_block):
                builder.branch(block)
            for argir, var in zip(func.ir_function.args, func.args + func.vars_closure + func.rets):
                frame.varptrs[var] = argir
            frame.impl_ast_body(body)
            builder.ret_void()
        return func


class Program:
    def __init__(self, name, target_machine):
        self.target_data = target_machine.target_data
        self._consts = {}
        self._function_asts = {}
        self.eval_const = baseeval.ConstExprEvaluator(self._consts)
        self.funcimpls = {}
        self.ir_module = ir.Module(name=name)
        self.forks_disabled = False

    def read_constants(self, nodes):
        for constdecl in nodes:
            with constdecl as (_1, ident, expr):
                self._consts[ident] = self.eval_const(expr)

    def eval_shape(self, shapenode):
        # !!! ignoring mutable
        dims, signed, _ = shapenode
        return Shape(*[self.eval_const(d) for d in dims], signed=signed)

    def impl_top_body(self, body):
        self.top_func = \
            Function.from_ast(self, ast.Tuple("func", "top", (), (), body), None)

    def get_function_ast(self, name):
        frame, ast = self._function_asts[name]
        return ast

    # TODO: @reentry_guard
    def request_func(self, opname):
        if opname not in self.funcimpls:
            self._impl_func(opname)
        return self.funcimpls[opname]

    def _impl_func(self, name):
        try:
            parent, f_ast = self._function_asts[name]
        except KeyError:
            raise ast.BadInput("no such function: {:h}", name)
        self.funcimpls[name] = Function.from_ast(self, f_ast, parent)


def main():
    llvm.initialize()
    llvm.initialize_native_target()

    parser = argparse.ArgumentParser(
        description='Compile Fold source into LLVM IR'
    )

    parser.add_argument(
        'sources', metavar='sources', nargs='*', type=str,
        help='source file names'
    )
    parser.add_argument('-o', '--output', type=pathlib.Path)
    parser.add_argument('-x', '--output-optimized', type=pathlib.Path)
    parser.add_argument('-v', '--verbose', action='store_const', const=sys.stderr,
                        default=io.StringIO())
    parser.add_argument('-e', '--jit-exec', action='store_true')
    parser.add_argument('-O', '--opt-level', type=int, default=1)
    parser.add_argument('-z', '--size-level', type=int, default=2)
    parser.add_argument('-t', '--target', default=llvm.Target.from_default_triple(),
                        type=llvm.Target.from_triple)
    parser.add_argument('-V', '--version', action='store_true')
    parser.add_argument('--disable-forks', action='store_true')

    args = parser.parse_args()

    if args.version:
        import llvmlite
        print("Fold machine code compiler", file=sys.stderr)
        print(f"LLVM version: {llvm.llvm_version_info!r}", file=sys.stderr)
        print(f"llvmlite version: {llvmlite.__version__}", file=sys.stderr)
        print(f"Python version: {sys.version}", file=sys.stderr)
        sys.exit(0)

    if not len(args.sources):
        sys.stderr.write("No sources specified\n")
        sys.exit(1)

    try:
        top_ast_nodes = [
            node
            for fname in args.sources
            for node in ast.parse_spec_from_file(fname)
        ]
    except IOError as e:
        print(f"{parser.prog}: error: {e.filename}: {e.strerror}",
              file=sys.stderr)
        sys.exit(1)
    except ast.BadInput as e:
        ast.print_code_snippet(e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)

    def filter_nodes(typ):
        return [node for node in top_ast_nodes if node[0] == typ]

    target_machine = args.target.create_target_machine()
    d = Program(args.sources[0], target_machine)
    d.forks_disabled = args.disable_forks

    try:
        d.read_constants(filter_nodes("const"))
        d.impl_top_body(top_ast_nodes)
    except ast.BadInput as e:
        ast.print_code_snippet(e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)

    if args.output:
        with args.output.open("w") as f:
            f.write(str(d.ir_module))

    if args.jit_exec:
        from ctypes import CFUNCTYPE
        llvm.initialize()
        llvm.initialize_native_target()
        llvm.initialize_native_asmprinter()

        print("Parsing...", file=args.verbose)

        module = llvm.parse_assembly(str(d.ir_module))
        module.verify()

        print("Optimizing...", file=args.verbose)

        pm = llvm.create_module_pass_manager()
        pmb = llvm.create_pass_manager_builder()
        pmb.opt_level = args.opt_level
        pmb.size_level = args.size_level
        pmb.populate(pm)
        pm.run(module)

        if args.output_optimized:
            with args.output_optimized.open("w") as f:
                f.write(str(module))

        print("Emitting...", file=args.verbose)

        target_machine = llvm.Target.from_default_triple() \
                            .create_target_machine()
        jit_compiler = llvm.create_mcjit_compiler(module,
                                            target_machine)
        jit_compiler.finalize_object()
        jit_compiler.run_static_constructors()
        func_ptr = jit_compiler.get_function_address("top")
        cfunc = CFUNCTYPE(None)(func_ptr)
        print("Running...", file=args.verbose)
        signal.signal(signal.SIGINT, signal.SIG_DFL)
        signal.signal(signal.SIGPIPE, signal.SIG_DFL)
        cfunc()
        print("Done", file=args.verbose)

if __name__ == "__main__":
    main()
