# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

import argparse
import pathlib
import sys
from io import StringIO

from . import rtl
from .rtl import ys # yosys
from .design import Design, do_dumps
from ..ast import BadInput, parse_spec_from_buffer, print_code_snippet

try:
    ys.Frontend
except AttributeError:
    print("This Yosys build doesn't support Python frontends", file=sys.stderr)
    sys.exit(1)

class FrontendArgparser(argparse.ArgumentParser):
    def exit(status=0, message=""):
        pass

class FoldFrontend(ys.Frontend):
    def __init__(self):
        super().__init__("fold", "Compile Fold source")
        self.parser = parser = argparse.ArgumentParser(
            prog=self.pass_name,
            add_help=False,
            exit_on_error=False,
            description='''
            Read Fold source and construct an intermediate circuit representation
            that can be synthesized into a digital circuit with the 'fold_synth' pass.
            '''
        )
        parser.add_argument('-b', '--dump-blockimpls', type=pathlib.Path,)
        parser.add_argument('-D', '--dump-immutlinks', type=pathlib.Path,)
        parser.add_argument('-d', '--dump-mutlinks', type=pathlib.Path,)
        #parser.add_argument('-t', '--top', help='Name of the generated top module',
        #                   type=str, default='top')
        parser.add_argument('-W', '--execid-width', help='Set width of ExecID tags',
                            type=int, default=128)

    def py_help(self):
        ys.log(self.parser.format_help() + "\n")

    def py_execute(self, f, filename, rawargs, design):
        try:
            args, extraargs = self.parser.parse_known_args(rawargs[1:])
        except argparse.ArgumentError as e:
            self.cmd_error(rawargs, 0, str(e))
        self.extra_args(f, filename, rawargs[:1] + extraargs, 1)

        try:
            top_ast_nodes = parse_spec_from_buffer(ys.read_istream(f), filename)
        except BadInput as e:
            print_code_snippet(e.markers)
            print(e, file=sys.stderr)
            sys.exit(1)

        def filter_nodes(typ):
            return [node for node in top_ast_nodes if node[0] == typ]

        rtl_design = rtl.Design.from_ys(design)
        d = Design(rtl_design.add_module(f"\\top"))
        d.execid_width = args.execid_width

        try:
            d.read_constants(filter_nodes("const"))
            d.impl_top_body(top_ast_nodes)
        except BadInput as e:
            print_code_snippet(e.markers)
            print(e, file=sys.stderr)
            sys.exit(1)

        do_dumps(args, d)

        try:
            d.build()
            d.rtl_module.ym.fixup_ports()
        except BadInput as e:
            print_code_snippet(e.markers)
            print(e, file=sys.stderr)
            sys.exit(1)
        return

    def py_clear_flags(self):
        pass

p = FoldFrontend()
