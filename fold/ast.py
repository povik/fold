# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import itertools
import string
import sys
import io
from contextlib import contextmanager


def markers_str(markers):
    if markers is None:
        return ""

    a, b = markers

    # TODO: in what conditions can this happen?
    if a is None or b is None:
        return ""

    if a.name != b.name:
        return "%s-%s" % (a, b)
    elif a.line != b.line:
        return "%s:%d:%d - %d:%d" % (a.name, a.line, a.col, b.line, b.col)
    elif a.col != b.col:
        return "%s:%d:%d-%d" % (a.name, a.line, a.col, b.col)
    else:
        return "%s:%d:%d" % (a.name, a.line, a.col)


def print_line_span(f, lines, a, b, highlight=False):
    hl = '\033[38;5;126m|\033[0;0m' if highlight else ' '
    for i in range(a - 1, b):
        if i < 0 or i >= len(lines):
            continue
        print(" \033[2;37m{:4d}\033[0;0m{}{}".format(i + 1, hl, lines[i].rstrip()), file=f)


def print_code_snippet(f, markers, inject_buffer=None):
    if markers is None or (markers[0] is None and markers[1] is None):
        return

    if markers[0] is None:
        markers = (markers[1], markers[1])
    if markers[1] is None:
        markers = (markers[0], markers[0])

    if markers[0].name != markers[1].name:
        return

    if inject_buffer is None:
        try:
            lines = list(open(markers[0].name, 'r'))
        except FileNotFoundError:
            return
    else:
        lines = list(io.StringIO(inject_buffer))

    a, b = markers

    print(file=f)
    if a.line != b.line:
        print_line_span(f, lines, a.line - 2, a.line - 1)
        if b.line - a.line >= 8:
            print_line_span(f, lines, a.line, a.line + 2, highlight=True)
            print("      ...", file=f)
            print_line_span(f, lines, b.line - 2, b.line, highlight=True)
        else:
            print_line_span(f, lines, a.line, b.line, highlight=True)
        print_line_span(f, lines, b.line + 1, b.line + 2)
    else:
        if a.line > len(lines):
            return
        print_line_span(f, lines, a.line - 1, a.line)
        line = lines[a.line - 1]
        print("      " + "".join([" " if (c != "\t") else "\t" for c in line[:a.col-1]])
                  + '\033[38;5;126m'
                  + ("~" * (max(b.col - a.col, 1)))
                  + '\033[0;0m', file=f)
        print_line_span(f, lines, a.line + 1, a.line + 1)
    print(file=f)


class _BadInputMessageFmt(string.Formatter):
    def format_field(self, value, format_spec):
        if format_spec.endswith("h"):
            return "\033[1;37m" \
                + super().format_field(value, format_spec[:-1]) \
                + "\033[0;0m"
        else:
            return super().format_field(value, format_spec)


class BadInput(Exception):
    FMT = _BadInputMessageFmt()

    def __init__(self, fmtstring, *args, **kwargs):
        self.msg = self.FMT.format(fmtstring, *args, **kwargs)
        self.markers = kwargs.get('markers', None) or Tuple.curr_markers

    def __str__(self):
        if self.markers is not None:
            #return "\033[2;37m%s\033[0;0m: %s" % (self.markers[0], self.msg)
            return "%s: %s" % (self.markers[0], self.msg)
        else:
            return self.msg


prefix_ops = list("!-~")
# TODO: postfix not implemented in parse_expr_part
postfix_ops = []
bi_ops_by_precedence = [
    list("."),
    ["**"],
    list("*%") + ["//", "/"],
    list("+-"),
    [">>", "<<"],
    list("<>") + [">=", "<="],
    ["==", "!="],
    ["&"],
    ["^"],
    ["|"],
    ["&&"],
    ["||"]
]
bi_ops = [item for sublist in bi_ops_by_precedence \
          for item in sublist]


class Expr:
    SPECIAL_OPNAMES = list("{~*+-&|^%<>") + ["<<", ">>", "==", "!=", "<=", ">=",
                                             "//", "[", "!", "||", "&&", "?:"]

    def __init__(self):
        self.markers = (None, None)

    def __enter__(self):
        # TODO: this needs some generalization
        Tuple.markers_stack.append(Tuple.curr_markers)
        Tuple.curr_markers = self.markers
        return self

    def __exit__(self, _a, _b, _c):
        Tuple.curr_markers = Tuple.markers_stack.pop()
        return False

    def replace(self, subs):
        if self in subs:
            return subs[self]
        else:
            return self


class Const(Expr):
    def __init__(self, val):
        Expr.__init__(self)
        self.val = val

    def __str__(self):
        return str(self.val)

    def __repr__(self):
        return "Const(%r)" % self.val


class Special(Expr):
    def __init__(self, val):
        Expr.__init__(self)
        self.val = val

    def __str__(self):
        return f"/{self.val!r}/"

    def __repr__(self):
        return f"Special({self.val!r})"


class Var(Expr):
    def __init__(self, varname):
        Expr.__init__(self)
        self.varname = str(varname)

    def __str__(self):
        return self.varname

    def __repr__(self):
        return "Var(%r)" % self.varname

    def __hash__(self):
        return hash(('var', self.varname))

    def __eq__(self, other):
        return isinstance(other, Var) \
            and self.varname == other.varname


class Op(Expr):
    def __init__(self, opname, args):
        Expr.__init__(self)
        self.opname = opname
        assert isinstance(args, list) or isinstance(args, tuple)
        self.args = args

        if len(args) > 0:
            self.markers = (args[0].markers[0], args[-1].markers[1])
        else:
            self.markers = (None, None)

    def _fmt_args(self):
        return ", ".join([str(arg) for arg in self.args])

    def __str__(self):
        if self.opname in prefix_ops and len(self.args) == 1:
            return self.opname + str(self.args[0])
        if self.opname in bi_ops and len(self.args) == 2:
            return str(self.args[0]) + self.opname + str(self.args[1])
        return self.opname + "(" + self._fmt_args() + ")"

    def __repr__(self):
        return "Op(%r, %r)" % (self.opname, self.args)

    def replace(self, subs):
        return Op(self.opname, [a.replace(subs) for a in self.args])


class SourceMarker:
    def __init__(self, line, col, name):
        self.hint_text = ""
        self.name, self.line, self.col = name, line, col

    def __str__(self):
        return "%s:%d:%d" % (self.name, self.line, self.col)

    def __repr__(self):
        return "<marker %s>" % str(self)

    def copy(self):
        ret = SourceMarker(self.line, self.col, self.name)
        ret.hint_text = self.hint_text
        return ret


class peekable_iter:
    def __init__(self, iter):
        self._orig = iter
        self.a, self.b = itertools.tee(iter)
        self._set_peek()

    def _set_peek(self):
        try:
            self._peek = next(self.b)
        except StopIteration:
            self._peek = None

    def peek(self):
        return self._peek

    def __iter__(self):
        return self

    def __next__(self):
        self._set_peek()
        return next(self.a)


class linecol_tracking_peekable_iter(peekable_iter):
    def __init__(self, iter, source_name):
        peekable_iter.__init__(self, iter)
        self.line, self.col, self.source_name = 1, 1, source_name

    def marker(self):
        return SourceMarker(self.line, self.col, self.source_name)

    def __next__(self):
        m = peekable_iter.__next__(self)
        if m is None:
            return m
        if m == "\n":
            self.col = 1
            self.line += 1
        else:
            self.col += 1
        return m


special = bi_ops + prefix_ops + list("[](),{};=") + [".", "..", ":", "?", "<-", "->"]
num = "0123456789"
alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
identfirst = alpha + "_$"
ident = identfirst + num + "'!"


def is_ident(tok):
    return isinstance(tok, str) and len(tok) > 0 \
           and tok[0] in identfirst \
           and all((c in ident for c in tok))


def tokens(chars, input_name="input"):
    inp = linecol_tracking_peekable_iter(chars, input_name)

    def consume(accepted, limit=None):
        ret = []
        while inp.peek() is not None \
                and inp.peek() in accepted \
                and (limit is None or len(ret) < limit):
            ret.append(next(inp))
        return "".join(ret)

    def string_literal():
        opening_marker = inp.marker()
        opening_quote = next(inp)
        assert opening_quote in "'\""
        if opening_quote == "'" and inp.peek() is not None and inp.peek() == "'":
            next(inp)
            if inp.peek() is None or inp.peek() != "'":
                return "''"
            next(inp)
            buffer = ""
            while len(buffer) < 3 or buffer[-3:] != "'''":
                if inp.peek() is None:
                    raise BadInput("missing termination of a triple-quoted literal",
                                   markers=(opening_marker, inp.marker()))
                buffer += next(inp)
            return "'" + buffer[:-3] + "'"
        ret = []
        while inp.peek() is not None \
                and inp.peek() != opening_quote:
            if inp.peek() != "\\":
                ret.append(next(inp))
            else:
                start = inp.marker()
                next(inp)
                verb = next(inp)
                if verb is None:
                    break
                elif verb == "\n":
                    continue
                elif verb in "\\'\"":
                    ret.append(verb)
                elif verb == "a":
                    ret.append("\a")
                elif verb == "b":
                    ret.append("\b")
                elif verb == "f":
                    ret.append("\f")
                elif verb == "n":
                    ret.append("\n")
                elif verb == "r":
                    ret.append("\r")
                elif verb == "t":
                    ret.append("\t")
                elif verb == "v":
                    ret.append("\v")
                elif verb == "o":
                    val = consume("01234567", limit=3)
                    if len(val) != 3:
                        raise BadInput("bad escape sequence", markers=(start, inp.marker()))
                    ret.append(chr(int(val, base=8)))
                elif verb == "x":
                    val = consume("0123456789abcdef", limit=2)
                    if len(val) != 2:
                        raise BadInput("bad escape sequence", markers=(start, inp.marker()))
                    ret.append(chr(int(val, base=16)))
                else:
                    ret.append(f"\\{verb}")
        if inp.peek() != opening_quote:
            raise BadInput("missing a closing quote for a string literal",
                           markers=(opening_marker, inp.marker()))
        next(inp)
        return "'" + "".join(ret) + "'"

    def consume_ident():
        ret = []
        while inp.peek() is not None \
                and inp.peek() in ident:
            ret.append(next(inp))
        if inp.peek() == ":":
            ret.append(next(inp))
        return "".join(ret)

    def convert_to_number(s):
        s = "".join([c for c in s if c != "_"])
        if False and '.' in s: # disabled
            return float(s)
        elif s.startswith("0x"):
            return int(s, base=16)
        elif s.startswith("0b"):
            return int(s, base=2)
        else:
            return int(s, base=10)

    def specialf():
        ret = ""
        while inp.peek() is not None \
                and any((x.startswith(ret + inp.peek()) \
                         for x in special)):
            ret += next(inp)
        if ret not in special:
            raise ValueError("token '%s' not understood" % ret)
        return ret

    specials_first_chars = "".join(list(set([s[0] for s in special])))

    token_types = {
        specials_first_chars: specialf,
        "0123456789": (lambda: convert_to_number(consume(num+"_abcdefx"))),
        identfirst: consume_ident,
        "'\"": string_literal
    }

    hint_text = ""

    def skipover(kind):
        hit = False
        while inp.peek() is not None \
                and inp.peek() in kind:
            next(inp)
            hit = True
        return hit

    def readuntil(kind):
        ret = ""
        while inp.peek() is not None \
                and inp.peek() not in kind:
            ret += next(inp)
        return ret

    preceding_ws = False
    following_ws = False
    while True:
        if skipover(" \t\r\n"):
            preceding_ws = True
        if inp.peek() == '#':
            next(inp)
            readuntil("\r\n")
            continue
        if inp.peek() == '`':
            next(inp)
            hint_text += readuntil("\r\n")
            continue

        found_token = False

        start = inp.marker()
        start.hint_text = hint_text
        start.preceding_ws = preceding_ws
        start.following_ws = False
        hint_text = ""
        preceding_ws = False

        if inp.peek() is None:
            break

        for k in token_types.keys():
            if inp.peek() not in list(k):
                continue

            try:
                tok = token_types[k]()
            except ValueError as e:
                raise BadInput("{}", e.args[0], markers=(start, inp.marker()))

            end = inp.marker()
            found_token = True
            break

        if not found_token:
            raise BadInput("unexpected character '{:h}'", next(inp),
                           markers=(start, inp.marker()))

        if skipover(" \t\r\n"):
            preceding_ws = True
            following_ws = True
        if inp.peek() == '#':
            next(inp)
            readuntil("\r\n")
            skipover("\r\n")
        if inp.peek() == '`':
            next(inp)
            hint_text += readuntil("\r\n")
            skipover("\r\n")

        end.hint_text = hint_text
        end.preceding_ws = False
        end.following_ws = following_ws
        following_ws = False
        yield (tok, start, end)


class TokenInput:
    def __init__(self, tokenizer):
        self.inp, self.peek_inp = itertools.tee(tokenizer)
        self._peek, self._peek_beg, self._peek_end = None, None, None
        self._do_peek()

        self.tried = []

    def _do_peek(self):
        try:
            self._peek, self._peek_beg, self._peek_end = next(self.peek_inp)
        except StopIteration:
            self._peek = None

    def __iter__(self):
        return self

    def __next__(self):
        self._do_peek()
        self.tried = []
        ret, a, b = next(self.inp)
        self.last_markers = (a, b)
        self.last = ret
        return ret

    def try_peek(self):
        return self._peek

    def try_kind(self, kind):
        if callable(kind):
            if kind(self._peek):
                return next(self)
            else:
                name = kind.__name__
                if name.startswith("is_"):
                    name = name[3:]
                self.tried.append(name)
        else:
            if self._peek == kind:
                return next(self)
            else:
                self.tried.append(kind)

    def consume(self, kind):
        return self.try_kind(kind) is not None

    def require(self, kind):
        if not self.consume(kind):
            self.abort()

    @classmethod
    def _text_for_kind(self, t):
        if len(t) == 1:
            return "%s" % t
        else:
            return t

    def abort(self):
        raise BadInput(
            "syntax error, encountered {} but expected one of: {}",
            f"'{self.try_peek()}'" if self.try_peek() is not None else "end-of-file",
            " ".join(map(self._text_for_kind, self.tried)),
            markers=(self._peek_beg, self._peek_end)
        )

    def pos_fwd(self):
        return self._peek_beg

    def pos_back(self):
        return self.last_markers[1]


def _get_biop_prec(op):
    return -[(op in ops) for ops \
            in bi_ops_by_precedence].index(True)


def _cmp_prec(a, b):
    return _get_biop_prec(a) - _get_biop_prec(b)


def is_atom(tok):
    return is_ident(tok) or isinstance(tok, int) \
           or isinstance(tok, float) or isinstance(tok, complex) \
           or (isinstance(tok, str) and tok.startswith("'"))


def is_number(tok):
    return isinstance(tok, int) \
           or isinstance(tok, float) or isinstance(tok, complex)


def is_prefix_op(tok):
    return tok in prefix_ops


def is_binary_op(tok):
    return tok in bi_ops


def mark_positions(f):
    def wrapper(t, *args, **kwargs):
        pre = t.pos_fwd()
        ret = f(t, *args, **kwargs)
        post = t.pos_back()
        ret.markers = (pre, post)
        return ret
    return wrapper


@mark_positions
def parse_expr_part_pre(t):
    if t.consume("("):
        val = parse_expr(t)
        t.require(")")
    elif t.consume("{"):
        m = []
        first = True
        while first or t.consume(","):
            m.append(parse_expr(t))
            first = False
        t.require("}")
        return Op(opname="{", args=m)
    elif t.consume(is_prefix_op):
        return Op(opname=t.last, args=[parse_expr_part(t)])
    elif t.consume(is_ident):
        name = t.last
        channel = False
        if t.consume("<-"):
            name += "<-"
            channel = True
        elif t.consume("->"):
            name += "->"
            channel = True

        if channel and t.consume("$nonblocking"):
            name += "$nonblocking"

        if t.consume("("):
            args = []
            done = False
            while (not args) or t.consume(","):
                if t.consume(")"):
                    done = True
                    break
                args.append(parse_expr(t))
            if not done:
                t.require(")")
            val = Op(name, args)
        else:
            if channel:
                t.abort()
            val = Var(name)
    elif t.consume(is_atom):
        val = Const(t.last)
    else:
        t.abort()
    return val


@mark_positions
def parse_index(t):
    arg = parse_expr(t)
    if t.consume(":"):
        arg2 = parse_expr(t)
        return Op(":", [arg, arg2])
    elif t.consume(".."):
        arg2 = parse_expr(t)
        return Op("..", [arg2, arg])
    else:
        return arg


@mark_positions
def parse_expr_part(t):
    val = parse_expr_part_pre(t)

    if t.consume("["):
        args = []
        while (not args) or t.consume(","):
            args.append(parse_index(t))
        t.require("]")
        return Op("[", [val,] + args)
    else:
        return val


@mark_positions
def parse_expr_biop_string(t):
    def finish(stack, atom):
        for s in stack:
            atom = Op(opname=s[1], args=[s[2], atom])
        return atom

    # stack is from highest to lowest precedence
    stack = []

    while True:
        atom = parse_expr_part(t)
        if not t.consume(is_binary_op):
            return finish(stack, atom)
        biop = biop_base = t.last

        if t.last_markers[0].preceding_ws \
                and not t.last_markers[1].following_ws:
            t.require(is_ident)
            biop = biop_base + t.last

        # if there are unfinished biops with same or higher prec,
        # finish those first then start a new biop
        higher_prec = filter(lambda a: _cmp_prec(a[0], biop) >= 0, stack)
        atom = finish(higher_prec, atom)

        lower_prec = filter(lambda a: _cmp_prec(a[0], biop) < 0, stack)
        stack = [(biop_base, biop, atom)] + list(lower_prec)


@mark_positions
def parse_expr(t):
    biop_expr = parse_expr_biop_string(t)
    if t.consume("?"):
        expr1 = parse_expr(t)
        t.require(":")
        expr2 = parse_expr(t)
        return Op("?:", [biop_expr, expr1, expr2])
    return biop_expr


def parse_expr_str(s):
    return parse_expr(TokenInput(tokens(s, None)))


@mark_positions
def parse_type(t, opening_consumed=False):
    shape = []
    if not opening_consumed:
        t.require("[")
    while not t.consume("]"):
        shape.append(parse_expr(t))
        if t.consume("]"):
            break
        else:
            t.require(",")
    signed, mutable = False, False
    while True:
        if t.consume("signed"):
            signed = True
            continue
        if t.consume("mut"):
            mutable = True
            continue
        break
    return Tuple(shape, signed, mutable)


def parse_named_type_list(t):
    ret = []
    argnames = []
    cont = True
    while cont:
        cont = False
        argtype = None
        t.require(is_ident)
        argname = t.last
        marker0 = t.last_markers[0]
        if t.consume("["):
            argtype = parse_type(t, opening_consumed=True)
        if t.consume(","):
            cont = True
        # TODO: here we should be more lenient in where the hints are placed
        marker1 = t.last_markers[1]
        argnames.append((argname, (marker0, marker1)))
        if argtype is not None:
            for argname, markers in argnames:
                ret.append(Tuple(argname, argtype, markers=markers))
            argtype = None
            argnames = []
    return ret


def parse_list(t, func, sep=",", end=None):
    ret = []
    first = True
    while first or t.consume(sep):
        if t.try_peek() == end: # TODO
            break
        ret.append(func(t))
        first = False
    return ret


def parse_ifelse_body(t):
    if t.consume("{"):
        stats = parse_statement_block(t)
        t.require("}")
    else:
        stats = [parse_statement(t)]
    return stats


def parse_proper_statement(t):
    expr = parse_list(t, parse_expr)
    if t.consume("="):
        lhs = expr
        rhs = parse_list(t, parse_expr)
        ret = Tuple("=", lhs, rhs)
    else:
        ret = Tuple(expr)
    return ret


def parse_ident(t):
    t.require(is_ident)
    return t.last


@mark_positions
def parse_statement(t, in_elif=False):
    if t.consume("chan"):
        return parse_chan(t)
    elif t.consume("func"):
        return parse_func(t)
    elif t.consume("goto"):
        t.require(is_ident)
        ret = Tuple("goto", t.last)
        t.require(";")
        return ret
    elif t.consume("fork"):
        t.require(is_ident)
        ret = Tuple("fork", t.last)
        t.require(";")
        return ret
    elif t.consume("break"):
        t.require(";")
        return Tuple("goto", "%break")
    elif t.consume("quit"):
        t.require(";")
        return Tuple("quit")
    #elif t.consume("continue"):
    #   t.require(";")
    #   return Tuple("goto", "%continue")
    elif t.consume("for"):
        condition = None
        if not t.consume("{"):
            condition = parse_expr(t)
        t.require("{")
        stats = parse_statement_block(t)
        t.require("}")
        return Tuple("for", condition, stats)
    elif t.consume("if") or in_elif:
        cond = parse_expr(t)
        true_body = parse_ifelse_body(t)
        if t.consume("else"):
            false_body = parse_ifelse_body(t)
        # TODO
        #elif t.consume("elif"):
        #   false_body = [parse_statement(t, in_elif=True)]
        else:
            false_body = None
        return Tuple("if", cond, true_body, (false_body if false_body is not None else []))
    elif t.consume("var"):
        ret = Tuple("var", parse_named_type_list(t))
        t.require(";")
        return ret
    elif t.consume("const"):
        ident = parse_ident(t)
        t.require("=")
        expr = parse_expr(t)
        t.require(";")
        return Tuple("const", ident, expr)
    elif t.consume("{"):
        ret = Tuple("{", parse_statement_block(t))
        t.require("}")
        return ret
    else:
        ret = parse_proper_statement(t)
        t.require(";")
        return ret


@mark_positions
def parse_label(t):
    return Tuple("label", next(t)[:-1])


def parse_statement_block(t, final="}"):
    ret = []
    opening_marker = t.pos_fwd()
    while t.try_peek() != final:
        if t.try_peek() is None:
            raise BadInput("unclosed statement block (opened at {:h})",
                           opening_marker,
                           markers=(t.pos_fwd(), t.pos_fwd()))
        if type(t.try_peek()) is str \
                and t.try_peek().endswith(":"):
            ret.append(parse_label(t))
            continue

        stat = parse_statement(t)
        if isinstance(stat, Tuple) and stat[0] == "var":
            # As a fixup transfer any hints from the top statement
            # node onto all of the individual variable declarations
            for entry in stat[1]:
                entry.markers[1].hint_text += f" {stat.markers[1].hint_text}"
        ret.append(stat)
    return ret


class Tuple:
    curr_markers = None
    markers_stack = []

    def __init__(self, *args, markers=(None, None)):
        self._a = args
        self.markers = markers

    def __iter__(self):
        return self._a.__iter__()

    def __hash__(self):
        return hash(self._a)

    def __eq__(self, other):
        return self._a == other._a

    def __len__(self):
        return len(self._a)

    def __getitem__(self, i):
        return self._a[i]

    def __repr__(self):
        return repr(self._a)

    @classmethod
    @contextmanager
    def clear_markers(self):
        save1, save2 = self.markers_stack, self.curr_markers
        self.markers_stack = []
        self.curr_markers = None
        try:
            yield
        finally:
            self.markers_stack = save1
            self.curr_markers = save2

    def __enter__(self):
        type(self).markers_stack.append(type(self).curr_markers)
        type(self).curr_markers = self.markers
        return self._a

    def __exit__(self, _a, _b, _c):
        type(self).curr_markers = type(self).markers_stack.pop()
        return False


def parse_func(t):
    if t.consume(is_binary_op) or t.consume(is_prefix_op):
        prefix = t.last
        t.require(is_ident)
        name = prefix + t.last
    else:
        t.require(is_ident)
        name = t.last

    markers0 = t.last_markers[0]
    args, rets = [], []

    if t.consume("("):
        if not t.consume(")"):
            args = parse_named_type_list(t)
            t.require(")")

    if t.consume("("):
        if not t.consume(")"):
            rets = parse_named_type_list(t)
            t.require(")")
        else:
            rets = []
    else:
        rets = [Tuple("ret", parse_type(t), markers=(t.last_markers[1], t.last_markers[1]))]

    t.require("{")
    stats = parse_statement_block(t)
    t.require("}")

    markers1 = t.last_markers[1]

    return Tuple("func", name, args, rets, stats,
                 markers=(markers0, markers1))


@mark_positions
def parse_chan(t):
    t.require(is_ident); ident = t.last;
    args, rets = [], []

    t.require("(")
    if not t.consume(")"):
        args = parse_named_type_list(t)
        t.require(")")

    t.require("(")
    if not t.consume(")"):
        rets = parse_named_type_list(t)
        t.require(")")

    return Tuple("chan", ident, args, rets)


def parse_spec(t):
    ret = parse_statement_block(t, final=None)
    if t.try_peek():
        t.abort()
    return ret


def parse_statement_block_from_buffer(buffer, fn):
    return parse_statement_block(TokenInput(tokens(buffer, fn)))


def parse_spec_from_buffer(buffer, fn):
    return parse_spec(TokenInput(tokens(buffer, fn)))


def parse_expr_from_buffer(buffer, fn):
    return parse_expr(TokenInput(tokens(buffer, fn)))


def parse_spec_from_file(fn):
    return parse_spec(TokenInput(tokens(open(fn).read(), fn)))


def main():
    try:
        input_fn = sys.argv[1] if len(sys.argv) >= 2 else "/dev/stdin"
        print(parse_spec_from_file(input_fn))
    except BadInput as e:
        print(e)


if __name__ == "__main__":
    main()
