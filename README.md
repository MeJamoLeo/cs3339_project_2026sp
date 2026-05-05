# MIPS Pipelined Simulator

A CS3339 project. This is a 5-stage pipelined MIPS simulator written in Common Lisp (SBCL).

The simulator reads an assembly file, runs it through the IF / ID / EX / MEM / WB stages cycle by cycle, and prints the final state of the registers and the data memory.

## Requirements

- SBCL (Steel Bank Common Lisp)
- If you use Nix, run `nix develop` to get everything you need (`flake.nix` is included).

## How to Run

There are two options.
- From the shell
- From the REPL

Also, there is a debug mode, which shows not only final state, but also snapshots from every single cycle.
Use option `:debug t` such that `(demo "./inputs/all" :debug t)`. The examples are following below.

### For combined instructions

From shell,

```sh
sbcl --non-interactive --load demo.lisp --eval '(demo "./inputs/all" :debug t)'
```

or from REPL

```lisp
(load "demo.lisp")
(demo "./inputs/all")              ; final state only (default)
(demo "./inputs/all" :debug t)     ; debug output for every cycle + final state
```

### For category-based instructions

There are multiple options based on categories, check the files in `./inputs/category/`

From shell,

```sh
sbcl --non-interactive --load demo.lisp --eval '(demo "./inputs/category/arith" :debug t)'
```

or from REPL

```lisp
(load "demo.lisp")
(demo "./inputs/category/arith")              ; final state only (default)
(demo "./inputs/category/arith" :debug t)     ; debug output for every cycle + final state
```

### For single instruction
There are each instruction demo, check the files in `./inputs/single/`

From shell,

```sh
sbcl --non-interactive --load demo.lisp --eval '(demo "./inputs/single/addi" :debug t)'
```

or from REPL

```lisp
(load "demo.lisp")
(demo "./inputs/single/addi")              ; final state only (default)
(demo "./inputs/single/addi" :debug t)     ; debug output for every cycle + final state
```


## Tests

From shell,

```sh
sbcl --non-interactive --load test.lisp
```

or from REPL

```lisp
(load "test.lisp")
```

In `test.lisp`, the `main.lisp` file is loaded at beginning.

## Project Layout

```
.
├── main.lisp          ; assembler + pipeline core
├── demo.lisp          ; entry point
├── debug.lisp         ; helpers that print the state of each cycle
├── test.lisp          ; unit tests
├── doc/               ; report (LaTeX / PDF)
└── inputs/
    ├── all            ; one file that uses every instruction
    ├── category/      ; simulations grouped by category
    │   ├── arith         (arithmetic)
    │   ├── branch        (branch)
    │   ├── jump          (jump)
    │   ├── logic-shift   (logic / shift)
    │   └── memory        (load / store)
    └── single/        ; one simulation per instruction
        ├── add, addi, and, beq, j, lw, mul,
        └── nop, or, sll, srl, sub, sw
```

## Supported Instructions

- R-type: `add`, `sub`, `and`, `or`, `sll`, `srl`, `mul`
- I-type: `addi`, `lw`, `sw`, `beq`
- J-type: `j`
- Other: `nop`

## What This Project Does Not Cover

- Hazard detection unit
- Forwarding unit
- Branch-taken flush
- Labels in assembly
- Floating-point instructions and FP registers
- ALU overflow / carry flags
- Cache hierarchy (memory is a flat one-cycle store)

Because there is no hazard handling, this project must insert `nop` instructions
by hand between dependent instructions. Two `nop`s are enough thanks to
the half-cycle write-back (WB writes the register file before ID reads
it in the same cycle).
