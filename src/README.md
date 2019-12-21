# Structure + Flow
After the source text is parsed (`sumo_lexer.mll` and `sumo_parser.mly`), it produces
an `Ast.compilation_unit`.

The old, *wrong* way to then get to LLVM IR was to pass the AST into `Llvm_compiler.compile`,
which effectively combined semantic analysis and code generation into one step. While this
was feasible, it made it vastly more complicated, and also required a lot of comments.

Now, the two phases are broken up. First, a `Ssa.universe` is created, and then passed to
`Ssa_of_ast.ssa_of_ast`, which compiles all functions in the compilation unit into an
intermediate representation, and returns the updated universe and some other context.
All type checks are performed during this phase.

This universe is then passed to `Llvm_of_ssa.llvm_of_ssa`, which just emits an LLVM module
of everything we've produced.