# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from io import StringIO
import argparse
import pathlib

from . import rtl
from .design import Design, do_dumps
from ..ast import BadInput, parse_spec_from_file, print_code_snippet


def main():
    parser = argparse.ArgumentParser(
        description='Compile Fold program to logic'
    )

    parser.add_argument('sources', metavar='sources', nargs='+', type=str,
                        help='source file names')
    parser.add_argument('-o', '--output', type=pathlib.Path,
                        default='/dev/stdout')
    parser.add_argument('-g', '--debug', action='store_true', help='Enable Fold synthesis debug')
    parser.add_argument('-D', '--dump-immutlinks', type=pathlib.Path,)
    parser.add_argument('-d', '--dump-mutlinks', type=pathlib.Path,)
    parser.add_argument('-s', '--dump-immutlinks2', action='store_true',)
    parser.add_argument('-i', '--presynth-cmds', type=str, help='Inject Yosys commands ahead of Fold synthesis')
    parser.add_argument('-p', '--presynth-script', type=str, help='Run Yosys script ahead of Fold synthesis')
    parser.add_argument('-E', '--execid', help='Add ExecID instrumentation for full debugging',
                        action="store_true")
    parser.add_argument('-W', '--execid-width', help='Set width of ExecID tags',
                        type=int, default=128)
    parser.add_argument('-n', '--no-synth', help='Do not synthesize, keep IR produced by frontend',
                        action="store_true")

    args = parser.parse_args()

    try:
        top_ast_nodes = [
            node
            for fname in args.sources
            for node in parse_spec_from_file(fname)
        ]
    except IOError as e:
        print(f"{parser.prog}: error: {e.filename}: {e.strerror}",
              file=sys.stderr)
        sys.exit(1)
    except BadInput as e:
        print_code_snippet(e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)

    def filter_nodes(typ):
        return [node for node in top_ast_nodes if node[0] == typ]

    rtl_design = rtl.Design()
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
        d.rtl_module.set_top()
        d.rtl_module.ym.fixup_ports()
        if args.presynth_cmds:
            rtl.ys.run_pass(args.presynth_cmds, rtl_design.yd)
        if args.presynth_script:
            rtl.ys.run_frontend(args.presynth_script, "script", rtl_design.yd)
        if args.dump_immutlinks2:
            rtl.ys.run_pass("scratchpad -get immutlinks", rtl_design.yd)
        if not args.no_synth:
            rtl.ys.run_pass(f"plugin -i {str(pathlib.Path(__file__).resolve().parents[2])}/build/fold.so", rtl_design.yd)
            rtl.ys.run_pass(f"{'debug ' if args.debug else ''}fold_synth" + (" --execid" if args.execid else ""), rtl_design.yd)
        rtl_design.write_il(args.output)
        if not args.no_synth:
            rtl.ys.run_pass("timespent", rtl_design.yd)
    except BadInput as e:
        print_code_snippet(e.markers)
        print(e, file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
