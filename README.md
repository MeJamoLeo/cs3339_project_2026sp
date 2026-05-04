MIPS Assembler & Instruction Encoder (Common Lisp)

Overview

This project is a MIPS assembler and instruction encoder written in
Common Lisp.
It reads MIPS-style assembly instructions, parses them, and converts
them into their corresponding 32-bit binary machine code representation.

The project demonstrates how assembly instructions are translated into
machine-level encoding.

------------------------------------------------------------------------

Features

-   Parses MIPS assembly instructions from input
-   Converts instructions into binary machine code
-   Supports:
    -   R-type instructions
    -   I-type instructions
    -   J-type instructions
-   Includes register mapping
-   Test script for validating encoding
-   Runs in a reproducible Nix environment

------------------------------------------------------------------------

Supported Instructions

-   Arithmetic: add, sub, addi
-   Logical: and, or
-   Shift: sll, srl
-   Memory (encoding only): lw, sw
-   Control flow (encoding only): beq, j
-   nop

Note: Instructions are encoded into binary but not executed.

------------------------------------------------------------------------

Project Structure

. ├── doc/
├── input/
├── main.lisp
├── test.lisp
├── flake.nix
├── flake.lock
├── .gitignore
└── README.md

------------------------------------------------------------------------

Setup

1.  Install Nix
    https://nixos.org/download.html

2.  Enter Environment
    nix develop

------------------------------------------------------------------------

Running the Program

Run main program:
sbcl –script main.lisp

Run tests:
sbcl –script test.lisp

------------------------------------------------------------------------

Input Format

Instructions follow standard MIPS syntax and are stored in the input/
directory.

Example: addi $t0, $zero, 5 addi $t1, $zero, 10 add $s0, $t0, $t1 sll
$s4, $s0, 2

------------------------------------------------------------------------

Output

Each instruction is translated into its corresponding 32-bit binary
representation.

------------------------------------------------------------------------

How It Works

1.  Reads assembly instructions
2.  Cleans formatting
3.  Tokenizes instructions
4.  Maps registers to numeric values
5.  Encodes instruction types
6.  Outputs binary encoding

------------------------------------------------------------------------

Limitations

-   Does not execute instructions
-   No register state simulation
-   No memory model
-   No pipeline simulation

------------------------------------------------------------------------

Authors

-   Reo Tajiri
-   Daniel Garcia
