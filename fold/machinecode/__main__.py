# The Fold compiler
# Copyright 2023 Martin Povišer <povik@cutebit.org>
# Distributed under the terms of the ISC license, see LICENSE

from . import *

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
