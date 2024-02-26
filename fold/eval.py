# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from . import ast


def product(l):
    ret = 1
    for i in l:
        ret *= i
    return ret


class FieldSet:
    def __init__(self, number, fields={}):
        self.length = number
        self.fields = fields

    @classmethod
    def cast(self, val):
        if isinstance(val, FieldSet):
            return val
        elif isinstance(val, int):
            return FieldSet(val)
        else:
            raise NotImplementedError(val)

    @classmethod
    def _shifted_range(self, rang, offset):
        return range(offset + rang.start, offset + rang.stop, rang.step)

    @classmethod
    def _shifted_fields(self, fields, offset):
        return {
            k: [self._shifted_range(v[0], offset)] + v[1:]
            for k, v in fields.items()
        }

    @classmethod
    def _merge_fields(self, a, b):
        ret = dict()
        conflicts = set(a.keys()).intersection(set(b.keys()))
        for k in conflicts:
            span_a = product([len(r) for r in a[k]])
            span_b = product([len(r) for r in b[k]])

            if span_a > span_b:
                ret[k] = a[k]
            elif span_b > span_a:
                ret[k] = b[k]

        for k in set(a.keys()) - conflicts:
            ret[k] = a[k]

        for k in set(b.keys()) - conflicts:
            ret[k] = b[k]

        return ret

    def __add__(self, other):
        other = self.cast(other)
        return FieldSet(
            self.length + other.length,
            self._merge_fields(
                self.fields,
                self._shifted_fields(other.fields, self.length)
            )
        )

    def __radd__(self, other):
        return self.cast(other) + self

    def __mul__(self, other):
        assert isinstance(other, int)
        assert other > 0 # TODO: error message
        return FieldSet(
            self.length + other,
            {k: [range(0, other*self.length, self.length)] + v
             for k, v in self.fields.items()}
        )

    def __rmul__(self, other):
        return self*other

    def dump(self):
        print("total length: %d" % self.length)
        for k, v in self.fields.items():
            print("\t" + "\t".join(
                ["%d..%d (+%d)" % (r.stop-1, r.start, r.step) for r in v]
            ) + "\t%s" % k)

    @classmethod
    def max(self, a, b):
        return FieldSet(
            max(a.length, b.length),
            self._merge_fields(a.fields, b.fields)
        )


class ExprVisitor:
    def on_Op(self, expr):
        return expr

    def on_Var(self, expr):
        return expr

    def on_Const(self, expr):
        return expr

    def on_Special(self, expr):
        return expr

    def __call__(self, expr):
        with expr:
            if type(expr) is ast.Op:
                return self.on_Op(expr)
            elif type(expr) is ast.Var:
                return self.on_Var(expr)
            elif type(expr) is ast.Const:
                return self.on_Const(expr)
            elif type(expr) is ast.Special:
                return self.on_Special(expr)
            else:
                raise ValueError("Unknown expression node of type %s" % type(expr))


class _ExprEvaluator(ExprVisitor):
    def on_Op_builtin(self, expr):
        opname = expr.opname
        args = [self(node) for node in expr.args]

        if opname == "{":
            return args
        if opname == "~":
            return ~args[0]
        if opname == "*":
            return args[0]*args[1]
        if opname == "+":
            return args[0]+args[1]
        if opname == "-":
            if len(args) == 2:
                return args[0]-args[1]
            else:
                return -args[0]
        if opname == "<<":
            return args[0]<<args[1]
        if opname == ">>":
            return args[0]>>args[1]
        if opname == "&":
            return args[0]&args[1]
        if opname == "|":
            return args[0]|args[1]
        if opname == "^":
            return args[0]^args[1]
        if opname == "==":
            return args[0]==args[1]
        if opname == "!=":
            return args[0]!=args[1]
        if opname == "<":
            return args[0]<args[1]
        if opname == "<=":
            return args[0]<=args[1]
        if opname == ">":
            return args[0]>args[1]
        if opname == ">=":
            return args[0]>=args[1]
        if opname == "!":
            return int(not args[0])
        if opname == "||":
            return int(args[0] != 0 or args[1] != 0)
        if opname == "&&":
            return int(args[0] != 0 and args[1] != 0)
        if opname == "?:":
            return args[1] if args[0] else args[2]

        # these below need special treatment of zero case if we are
        # working with python integers
        if opname == "//":
            return args[0]//args[1]
        if opname == "%":
            return args[0]%args[1]

    def on_Op(self, expr):
        val = self.on_Op_builtin(expr)
        if val is not None:
            return val

        raise NotImplementedError("unimplemented: evaluation of %s" % expr)

    def on_unimplemented(self, expr):
        raise NotImplementedError

    on_Var = on_unimplemented
    on_Const = on_unimplemented
    on_Special = on_unimplemented


def index(v, indices):
    if len(indices) == 0:
        return v
    if isinstance(v, list):
        return index(v[indices[0]], indices[1:])
    if isinstance(v, int) and len(indices) == 1:
        return bool((v >> indices[0]) & 1)
    raise NotImplementedError


class ConstExprEvaluator(_ExprEvaluator):
    def __init__(self, constvals):
        self.constvals = constvals

    def on_Var(self, expr):
        varname = expr.varname

        if varname in self.constvals:
            return self.constvals[varname]
        else:
            raise ast.BadInput("no such constant or variable: '%s'" % varname)

    def on_Const(self, expr):
        return expr.val

    def on_Op(self, expr):
        opname, args = expr.opname, expr.args

        if opname == "log2ceil":
            return (self(args[0])-1).bit_length()

        if opname == "ctz":
            v = self(args[0])
            return (v & -v).bit_length() - 1

        if opname not in ast.Expr.SPECIAL_OPNAMES:
            raise ast.BadInput("function calls are unsupported in constant expressions")

        if opname == "[":
            return index(self(args[0]), [self(n) for n in args[1:]])
        if opname == "//":
            args = [self(a) for a in args]
            return (args[0]//args[1]) if args[1] != 0 else 0
        if opname == "%":
            args = [self(a) for a in args]
            return (args[0]%args[1]) if args[1] != 0 else 0

        return super().on_Op(expr)


if __name__ == "__main__":
    import argparse
    import sys
    parser = argparse.ArgumentParser(
        description='Run a .fold test against constant evaluator'
    )

    parser.add_argument(
        'sources', metavar='sources', nargs='*', type=str,
        help='source file names'
    )

    args = parser.parse_args()
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
        ast.print_code_snippet(sys.stderr, e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)

    try:
        for stat in top_ast_nodes:
            eval_ = ConstExprEvaluator({})
            with stat:
                if not isinstance(stat[0], list) or \
                        len(stat[0]) != 1 or not isinstance(stat[0][0], ast.Op) \
                        or stat[0][0].opname != "assert_equal" \
                        or len(stat[0][0].args) != 2:
                    raise ast.BadInput("only assert_equal statements are supported")
                lhs, rhs = stat[0][0].args
                if eval_(lhs) != eval_(rhs):
                    raise ast.BadInput("failed assertion")
    except ast.BadInput as e:
        ast.print_code_snippet(sys.stderr, e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)
