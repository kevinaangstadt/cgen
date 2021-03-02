# Simple Code Generator
## CS 364 Spring 2021
### St. Lawrence University

This repository contains starter code for a simple code generator written in ReasonML.

## Grammar
See [grammar.txt](grammar.txt) for the simple grammar.

## Basic Usage

A basic program for computing fibonacci numbers is provided in [fib.sim](fib.sim). Our goal is to create a code generator that can produce `a.cl-asm` (or `a.s` for "real" assembly") that can be executed.

Right now, the program will read in a program and print it back out:

```bash
dune build main.exe
./main.exe fib.sim
```