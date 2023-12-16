# Compiler for the Grace programming language implemented in OCaml

## Overview

The compiler works as follows:
    - Lexical analysis is the first step and is implemented using ocamllex
    - Parsing is done along with the lexical analysis and is implemented using Menhir
    - Semantic analysis is done alongside the parsing
    - After the entire AST has been created and the semantic analysis has concluded,
      it is translated into LLVM IR, using the OCaml LLVM bindings.
    - The IR is (optionally) optimized before being translated to the assembly used by the host machine
    - LLVM emits LLVM IR code (in a .imm file), host machine assembly code (in a .asm file),
      as well as an object file (in a .o file)
    - Clang is finally used to link the .o file with the Grace runtime library and the C runtime library
      and create the final executable (in a .exe file)
    - If any errors are encountered at any phase of the compilation,
      an error message is output to stderr and the compilation fails

In short, two intermediate representations are used, one that closely matches
the grammar of the language, and LLVM IR.

## Nested Functions

LLVM does not support nested functions, but Grace does.
In order to use LLVM for optimizing and translating to
assembly/machine code the following workaround is done:

$ Consider a simple Grace program:
fun f(): nothing
    var i: int;
    var c: char;
    fun g(arg: int)
        fun h(): nothing
        {
            c <- 'a';
        }
    {
        i <- 5;
        h();
    }
{
    g(2);
}

// The equivalent LLVM representation (written using C syntax) is:
struct struct_f {
    void* parent;
    int* i;
    char* c;
};

struct struct_g {
    void* parent;
    int* arg;
};

struct struct_h {
    void* parent;
};

void h(struct_g* parent) {
    *(parent->parent->c) = 'a';
}

void g(struct_f* parent, int arg) {
    *(parent->i) = 5;

    struct_g sg = {
        .parent = parent,
        .arg = &arg,
    };

    h(sg);
}

void f() {
    int i;
    char c;

    struct_f sf = {
        .parent = NULL,
        .i = &i,
        .c = &c,
    };

    g(sf, 2);
}
