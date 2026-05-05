# MIPS Simulator with Pipeline (Common Lisp)

## Overview

This project is a MIPS simulator written in Common Lisp.
It includes instruction parsing, binary encoding, and execution using both single-cycle and pipelined CPU models.

The simulator demonstrates how a processor handles instruction execution, control signals, memory access, and pipelined processing.

---

## Features

* Parses MIPS assembly instructions from input
* Encodes instructions into 32-bit machine code
* Executes instructions using:

  * Single-cycle execution model
  * 5-stage pipelined execution model
* Includes:

  * Register file (32 registers)
  * Data memory
  * ALU and ALU control logic
  * Control unit (opcode-based)
  * Branch and jump handling
* Pipeline stages:

  * IF (Instruction Fetch)
  * ID (Instruction Decode)
  * EX (Execute)
  * MEM (Memory)
  * WB (Write Back)

---

## Supported Instructions

* Arithmetic: `add`, `sub`, `addi`, `mul`
* Logical: `and`, `or`
* Shift: `sll`, `srl`
* Memory: `lw`, `sw`
* Control flow: `beq`, `j`
* `nop`

---

## Project Structure

```
.
├── main.lisp      # Core CPU + pipeline logic
├── demo.lisp      # Demo entry point
├── debug.lisp     # Debugging and visualization tools
├── test.lisp      # Unit + integration tests
├── input/         # Assembly programs
├── flake.nix      # Nix environment
├── flake.lock     # Locked dependencies
├── .gitignore     
└── README.md
```

---

## Setup

### 1. Install Nix

https://nixos.org/download.html

### 2. Enter Development Environment

```bash
nix develop
```

This sets up:

* SBCL (Common Lisp compiler)
* Lisp libraries (Swank, Alexandria, YASON, VOM, USocket)
* Vlime server

---

## Running the Simulator

### Run demo with pipeline output (recommended)

```bash
sbcl --non-interactive --load demo.lisp --eval '(demo "./input")'
```

### Run demo (final state only)

```bash
sbcl --non-interactive --load demo.lisp --eval '(demo "./input" :debug nil)'
```

### Run tests

```bash
sbcl --script test.lisp
```

---

## Example Input

```asm
addi $t0, $zero, 6
addi $t1, $zero, 4
nop
nop
add $s0, $t0, $t1
mul $s2, $t0, $t1
and $s3, $t0, $t1
or $s4, $t0, $t1
sll $s5, $t0, 2
srl $s6, $t0, 1
sub $s1, $s0, $t1
addi $sp, $zero, 64
nop
nop
sw $s0, 0($sp)
nop
nop
lw $s7, 0($sp)
nop
nop
beq $s0, $s7, 4
nop
nop
addi $t2, $zero, 99
addi $t3, $zero, 77
addi $t4, $zero, 1
j 29
nop
addi $t5, $zero, 88
addi $t8, $zero, 42
```

---

## Debugging & Visualization

The simulator includes a debugging module (`debug.lisp`) for detailed execution tracing.

### Features

* Cycle-by-cycle pipeline state output
* Displays pipeline registers:

  * IF/ID
  * ID/EX
  * EX/MEM
  * MEM/WB
* Shows control signals per stage
* Tracks:

  * Register updates
  * Memory updates
  * Program counter changes

### Final Output

* Non-zero registers
* Non-zero memory values
* Final program counter

---

## How It Works

### 1. Assembly Processing

* Reads and normalizes instructions
* Tokenizes input
* Encodes instructions into binary

### 2. Single-Cycle Execution

* Decodes instruction fields
* Generates control signals
* Executes via ALU
* Updates registers and memory
* Advances program counter

### 3. Pipeline Execution

* Implements 5-stage pipeline:

  * IF → ID → EX → MEM → WB
* Uses pipeline registers:

  * `*if-id*`, `*id-ex*`, `*ex-mem*`, `*mem-wb*`
* Handles:

  * Branch decisions in EX stage
  * Jump decisions in ID stage
* Runs until pipeline is fully drained

---

## Limitations

* No hazard detection or forwarding
* Requires manual NOP insertion for dependencies
* Simplified memory model
* No exception handling

---

## Reproducibility

This project uses a pinned Nix flake (`flake.lock`) to ensure a fully reproducible environment.

All dependencies are locked to a specific version of `nixpkgs`, allowing consistent builds across systems.

---

## Repository Configuration

This repository uses a restrictive `.gitignore` strategy:

* All files are ignored by default
* Only essential files are tracked:

  * `.lisp` source files
  * `input/` programs
  * Nix configuration files
  * `.gitignore`

This keeps the repository minimal and focused.

---

## Authors

* Reo Tajiri
* Daniel Garcia
