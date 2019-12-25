# sumo-lang
An attempt at writing a compiler, this time in OCaml. My goal is
to actually finish this project, by taking my time, writing good code,
writing a lot of comments, and finishing `TODO` items before adding new
features.

This is seriously nowhere close to done, so just pretend it doesn't exist.

## About
If Dart, Swift, and C++ had a weird lovechild, it would probably be this language. It's built
on LLVM, and therefore compiles to native code (or LLVM IR, or Assembly). Garbage collection is
the M.O. for memory recovery (news flash - not everybody is writing for embedded systems, and no,
I don't care if you are).

Some aims:
* Performance
* Ease of use, hence the GC usage and lack of borrow checker, smart pointers, etc.
* Readability + familiarity
* Easy C calling
* Overall, an enjoyable/productive developer experience, for systems and backend development.

## Structure
`sumoc` is your average compiler. Lexing, parsing, semantic analysis, and LLVM code generation are
performed sequentially.

By the time the core language is done, I will look into incremental compilation, and writing a
language server (these two things can likely share a sizeable amount of infrastructure).

## When will it be self hosting?
If Sumo gets enough traction, it'll be worthwhile to develop a self-hosting compiler for it. Having
all infrastructure available as Sumo libraries will make it easier to write tooling.

For example, Dart has its own `build` system, and the static analyzer for Dart is self-hosted. Because of
this, you can relatively easily write code generators and utilities that are aware of all the static information
the compiler can deduce.

## Usage
Basic examples:

```bash
sumoc --help
sumoc -O2 -c -o foo.o foo.sumo
sumoc -emit-llvm -o foo.ll foo.sumo
```

Note that `sumoc` is *not* a linker; use your C compiler (or just `ld`) for that. Make sure to
link against `libsumo.a`.

## Example
There is no such thing as a "string" yet, so at this point, `Hello world` means
declaring an external `putc` function (there's one in `libsumo`), and then linking
against `lsumo`, after manually emitting each character:

```sumo
external #sumo_glue_putc putc (ch: int) : void

fn main() : void {
  putc(72)
  putc(101)
  putc(108)
  putc(108)
  putc(111)
  putc(32)
  putc(119)
  putc(111)
  putc(114)
  putc(108)
  putc(100)
  putc(10)
  return
}
```

Really puts the *hell* in "Hello, world."

## Dependencies
* OCaml (duh), and by extension `opam`
* `llvm`, only tested so far with `9.0`
* `menhir`
* `ocamllex`

AFAICT the `opam pin` command should install these all; otherwise, just go
ahead and install them.

## Installation
Firstly, you need to get the compiler itself built and installed.
This package isn't published yet, so for now, you'll need to `opam pin` it:

```bash
git clone https://github.com/thosakwe/sumo-lang
cd sumo-lang
opam pin .
```

Next, run `make -j4 lib` to build `libsumo`.
Run `make -j4 testcases` to compile test case programs.

## Standard Library
At this point, the module system isn't implemented, so there isn't really
a standard library. However, Sumo programs can link against C "glue" code,
so this is the approach taken to build bindings against `libc`. The eventual
goal is for most of it to be written in Sumo, though.