# YMCA

YMCA (or You May Compile Anu) is the final version of the compiler,
written in Anu itself and (hopefully) with much less hacky bits.

## Constraints

Anu should be a good alternative for computers with constrained resources
like microcontrollers. This means that the backend will need to deal with
various weird architectures and produce a very small binary.

Some targets like the ATmega328P microcontroler needs the
final code binary to have less than 30 kilobytes, and have
microcontroler specific I/O pins. They also have asymetric
registers and weird quirks like needing 2 registers to form an address.
This information must be known to the assembler (and backend)
so it can be checked and can be used to generate the final binary.

Some things, like abstracting the I/O pins, are going to be deferred
to the user, he should be able to write assembly to interface with
hardware as he wishes, and the compiler should give as many guarantees
as possible.

## Architecture

The compiler must understand at least *part* of the target 
architecture, this piece of knowledge must exist in the compiler
both for object file generation and self validation.

We can modularize this target-specific knowledge into an
assembler library, this library will be completely independent
from the compiler, but will initially only have features required
by the compiler.

This will allow us to reuse these libraries later to allow the user
to write assembly language together with Anu code (not inline though),
or even for the user to create his own compilers.

The Assembler library interface must be simple, though not standardized,
it must have a structured in-memory type that represents the code,
together with a enum of all suported instructions. It must have a procedure
that takes an argument of this type and returns a possibly empty list of errors 
from the source. The following is a possible example for amd64:

```
export all

type Reg 
  enum rax, rbx, rcx, rdx, ...;
type Imm is i32
type Label is *i8
type Addr is {.operand Reg|Label .offset i32}

type Instruction
  enum Mov  is {Addr Addr|Reg|Imm},
       Add  is {Reg Reg|Imm},
       Sub  is {Reg Reg|Imm},
       Push is Reg|Imm|Label,
       ...;

type Block is {
  .label *i8
  .code *Instruction
}

type DataType
  enum Byte, Word, DWord, QWord

type DataContents
  enum 
    Reserved is {.type DataType .size i32},
    Declared is {.type DataType .contents (*i8|i32|i64)}

type Data is {.label *i8 .contents DataContents}

type Program is {
  .data *Data
  .code *Block
}

type DataError is {.label *i8 .problem *i8}
type CodeError is {.label *i8 .instr Instruction .problem *i8}
type error is DataError | CodeError

proc Check[Program] ?*error do
  ...;

# *i8 here just represents a blob
proc Assemble[p:Program] *i8 | *error do
  begin
    Check[p]^nil
    ...
  end
```

This module can then be imported by the backend, that will transform
LIR into this representation. There would be a single backend for 
each architecture.

```
                                                   -----------------  MDIR   -------------------
                                            -----> | arm64 backend | ------> | arm64 assembler | \
                                           /       -----------------         -------------------  \
------------   HIR   ------------   LIR   /   -----------------  MDIR    -------------------       \   object file
| Frontend | ----->  | Lowering | -------|--> | amd64 backend | -------> | amd64 assembler | ---------------------->
------------         ------------         \   -----------------          -------------------                  /
                                           \       ----------------------  MDIR    ------------------------  /
                                            -----> | ATmega328P backend | -------> | ATmega328P assembler | /
                                                   ----------------------          ------------------------
```

Each square above should be a separate module.

In the future an optimizing module may exist that processes HIR (or LIR)
and removes dead code, propagates constants, eliminates common expressions,
and other things, mostly because for microcontrollers every byte
removed from the source code counts.

When the time comes, the assembler should also understand CPU
specific architecture extensions (like for example AVX512), and the compiler
might need to be able to produce fat binaries that verify if the CPU has
support for such features, changing the implementation of some procedures
on startup.
