# anu programming language

Anu is a work-in-progress.

It is a procedural, statically typed programming language.
The most important characteristics are it's compile-time
memory management through use of unique pointers,
the lack of statements and the focus on algebraic data types.

```
proc main do print["Hello, World!\n"];
```

A few guiding principles of the design:

 - Compile-time complexity is better than runtime complexity
 - Compile-time guarantees are more important than language flexibility
 - Limited purpose: beeing simple is more important than being general
 - Correctness, Performance, Conciseness. In that order.

# Table of Contents

1. [Introduction](#introduction)
2. [Lexical Elements](#lexical)
    1.  [Identifier](#identifier)
    2.  [Keywords](#keywords)
    4.  [Int](#int)
    5.  [Bool](#bool)
    6.  [Char](#char)
    7.  [String](#string)
    8.  [Comment](#comment)
    9.  [Whitespace](#whitespace)
    10. [Operators and Ponctuation](#ponctuation)
3. [Grammatical Elements](#grammar)
    1. [Module](#module)
    2. [Header](#header)
        1. [Import](#import)
        2. [FromImport](#fromimport)
        3. [Export](#export)
    3. [Body](#body)
        1. [Procedure](#procedure)
        2. [Constant](#constant)
        3. [Type](#type)
            1. [Sums](#sums)
            2. [Products](#products)
            3. [References](#refs)
            4. [Procedure Types](#proceduretype)
            5. [Array Types](#arraytype)
            6. [Map Type](#maptype)
            7. [Optional](#optional)
            8. [Enum](#enum)
    4. [Expr](#expr)
        1. [Binary Operators](#binaryoperators)
        2. [Prefix](#prefix)
        3. [Suffix](#suffix)
            1. [Call](#call)
            2. [Property Access](#propertyaccess)
            3. [Array Indexing](#arrayaccess)
            4. [Bubble Up](#bubbleup)
    5. [Factor](#factor)
        1. [NestedExpr](#nestedexpr)
        2. [Block](#block)
        3. [Product Literal](#productliteral)
        4. [Array/Map Literal](#arraymapliteral)
        5. [For](#for)
            1. [Conditional](#conditional)
            2. [Iterative](#iterative)
            3. [Range](#range)
        6. [Switch](#switch)
            1. [Value Switch](#valueswitch)
            2. [Type Switch](#typeswitch)
        7. [If](#if)
        8. [EarlyReturn](#earlyreturn)
        9. [Let](#let)
        10. [Set](#set)
        11. [New](#new)
4. [Semantics](#semantics)
    1. [Type Canonicalization](#typecanonicalization)
    2. [Type Identity](#typeidentity)
    3. [Type Equivalence](#typeequivalence)
    4. [Assignable](#assignable)
    5. [Castable](#castable)
    6. [Immutable/Mutable](#immutablemutable)
    7. [Addressable](#addressable)
    8. [Moving Semantics](#movingsemantics)
    9. [Freeing Semantics](#freeingsemantics)
    10. [Scopes](#scopes)
5. [Misc](#misc)
    1. [Full Grammar](#fullgrammar)
    2. [Builtin Modules](#builtinmodules)
    3. [Future](#future)
6. [Examples](#examples)

# Introduction <a name="introduction"/>


# Lexical Elements <a name="lexical"/>

Anu code files are ascii encoded text.

## Identifier <a name="identifier"/>

```
letter = 'a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|
         'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|
         'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|
         'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|
         'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|
         'Y'|'Z'|'_'.
digits = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'.

id := letter {letter | digit}.
```

## Keywords <a name="keywords"/>

Some identifiers are reserved and have special meaning,
the following 36 identifiers are considered keywords and
cannot be used as symbol names.

```
import  from     export  proc  const   type   
begin   end      is      or    and     not    
do      for      if      each  in      switch 
case    default  elseif  else  return  let    
set     as       to      then  range   nil    
new     remove   true    false void   
```

## Int <a name="int"/>

```
hexDigits = '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|
      'A'|'B'|'C'|'D'|'E'|'F'|'a'|'b'|'c'|'d'|'e'|'f'.
binDigits = '0'|'1'.
numEnding = 'ss'|'s'|'l'|'ll'.
number := decimal | hexadecimal | binary.
decimal := digits {digits} [numEnding].
hexadecimal := '0x' hexDigits {hexDigits} [numEnding].
binary := '0b' binDigits {binDigits} [numEnding].
```

numEnding dictates the type of the literal:

 - `0ss` (shorter short) has type `i8`
 - `0s` (short) has type `i16`
 - `0l` (long) has type `i32`
 - `0ll` (longer long) has type `i64`
 - `0` has type `int`

## Bool <a name="bool"/>

These two are boolean literals and also considered reserved words:

```
  true  false
```

## Char <a name="char"/>

char represents an ascii character, type: `i8`.

```
escapes := '\\"' | '\\'' | '\\n' | '\\t' | '\\r'.
char := "'"(ascii|escapes)"'".
```

## String <a name="string"/>

string represents an ascii string, type: `*i8`.

```
string := '"'(ascii|escapes)*'"'.
```

## Comment <a name="comment"/>

There are only line comments in anu, they start with `#`
and end with a newline. They may be discarded during compilation
but parsers may use them for documentation.

```
# this is a comment
proc Square[a:int] int # so is this
  do a*a # and this
```

## Whitespace <a name="whitespace"/>

The characters `\t`, ` `, `\n` and `\r` are considered whitespace
and are discarded during lexing, they are only significant as a means
of separating tokens.

## Operators and Ponctuation <a name="ponctuation"/>

The following is a list of all 38 operators and ponctuation tokens
in the language. All of them, including `-=`, `+=`, etc, are treated
as a single token.

```
    ;    =    [    ]     (    )    {    }
    ,    :    |    ->    .    ?    &    \
    ::   ==   !=   >     <    <=   >=   +
    -    ..   *    /     %    @    ~    $
    ^    -=   +=    *=   ..=  <->
```

# Grammatical Elements <a name="grammar"/>

Anu's grammar is designed to be parsed by recursive descent, and
should be very close to LL(1). There are, however, places of
ambiguity for example with the dangling else, in all cases,
the grammatical elements always bind to the innermost production.
Consider the following `if` expression.

```
if true then
if true then 0
else 1
```

It should be parsed like so:

```
if true then (
  if true then 0
  else 1
)
```

Instead of:

```
if true then
  (if true then 0)
else 1
```

Where this rule is not convenient (or safe), it's best to
desambiguate with parenthesis `(`/`)` or blocks `begin`/`end`.

Productions known to be dangling are: `let ... in ...`, `if ... then` and
`switch ... case`. Productions that are not LL(1) are all lists with
optional trailing commas and fields with optional names.

## Module <a name="module"/>

```
Module = Header Body
```

In anu, a file is a whole module, modules can't expand through multiple
files. Each file has two parts: a header and a body. The header part
has information related to module dependency, while the body has symbol
information.

The name of an anu file must have a valid identifier up to the first
dot `.`, and must end with the `.anu` extension. The following is a list
of valid anu file names:

```
anu.anu
m0.anu
module.metadata.anu
_g3tg1d.anu
m.1234569.anu
```

While the following are invalid:

```
.anu
1.anu
-1.anu
module
to.anu   # keyword
proc.anu # keyword
```

Note: keywords can't be used as the name of a module

New types declared in separate modules are always different,
that is: `A::T` is different from `B::T` if both are not aliases.

The entry point of a module is a procedure named `main`, with type
`proc[] nil`, `proc[**i8] nil` or `proc[**i8] i8`.

## Header <a name="header"/>

```
Header = (ModuleRelations ";"?)*
ModuleRelations = Import | Export | FromImport
```

A module header contains a series of module relations,
these define what is imported and exported from this module.
Module relations can only appear in the top of a file and
nowhere else.

The module system architects module relations by using both
exclusive imports and exclusive exports, in other words,
you can choose what symbols to export from a module and 
you can choose what to import from a module.

Importing a module twice is an error, both with `from ... import ...`
and `import ...`. Exporting the same symbol twice is also an error.

### Import <a name="import"/>

```
Import = "import" AliasList
AliasList = Alias ("," Alias)* ","?
Alias = id ("as" id)?
```

The import construct imports a module into the namespace,
the module being imported must live in the same folder as
the current module. All exported symbols of the module are
available by using the `::` operator. Example:

```
import IO

const myWrite = IO::Write
const myRead  = IO::Read
```

It's possible to rename the module when importing:

```
import IO as io
```

### FromImport <a name="fromimport"/>

```
FromImport = "from" id "import" AliasList
```

The `from ... import ...` construct imports names from a module
directly into global scope, making it unnecessary to use the `::`
operator to access exported symbols. The module being imported
must live in the same folder as the current module. Example:

```
from IO import Read, Write

const myWrite = Write
const myRead = Read
```

The module is not directly accessible, that is: `IO`
is not present in the global namespace, only the chosen symbols.
It's also possible to rename symbols when importing:

```
from IO import Read as myRead, Write as myWrite
```

### Export <a name="export"/>

```
Export = "export" ("all" | AliasList)
```

The `export ...` construct makes a series of symbols public 
to other modules, that is: it exports them. Example:

```
export Pi, Tau
const Pi = 3
const Tau = 2*Pi
```

It's also possible to export `all` symbols without enumerating
them one by one, the following example has the same module
interface as the previous one.

```
export all
const Pi = 3
const Tau = 2*Pi
```

It's also possible to rename symbols when exporting using the `as`
keyword:

```
export Pi as Tau, Tau as Pi # mischievous
const Pi = 3
const Tau = 2*Pi
```

## Body <a name="body"/>

```
Body = (Symbol ";"?)*
Symbol = Constant | Procedure | Type
```

The body of a module is made of named symbols declared
possibly out of order. Cycles are only allowed in procedures and types.

### Procedure <a name="procedure"/>

```
Procedure = "proc" id TypeAnnot? Signature "do" Expr
Signature = (Arguments (Return)?)?
Arguments = "[" DeclList? "]"
Return = TypeExpr
DeclList = Decl ("," Decl)* ","?
Decl = id TypeAnnot?
TypeAnnot = ":" TypeExpr
```

A procedure is a special kind of constant that defines
a procedure interface and it's implementation. A procedure
may have zero or multiple arguments, but always a single return.
All arguments of a procedure are immutable and the compiler
may choose to pass them as values or references depending
whether he can be sure that's safe or not.

It's possible to declare a procedure by first specifying a type,
then naming the arguments, or by specifying each argument with
it's respective type, the following procedures have identical types:

```
type Binary as proc[int, int] int

proc Add:Binary [a, b] do a + b;
proc Sub[a:int, b:int] int do a - b;
```

It's also possible to omit certain parts of a procedure declaration,
the following procedure has type: `proc[]nil`

```
proc main do print["hello"]
```

If the return type of a procedure is not specified, the value of the
expression is discarded, and the type defaults to `nil`. Example:

```
proc F do
  for range 0 to 10 do
    print["Hello\n"];
```

If `print` has type `proc[*i8]nil` the whole expression has type `*nil`,
but since the value is discarded, `F` has type `proc[]nil`

If a return type is specified, the expression following `do`
must have type `assignable` to this return type.

### Constant <a name="constant"/>

```
Constant = "const" id TypeAnnot? "=" Expr
TypeAnnot = ":" TypeExpr
```

Constants are immutable global symbols initialized with static values.
In other words, the initial value may only contain deterministic operations
capable of being computed at compile time.

It's invalid to create a constant that uses blocks, branching, loops, 
mutation, addressing, returns, procedure calls and/or array/map accessing.

If a constant has it's type annotated, the initialization expression must
have type `assignable` to this type. If it's not annotated, the type of the
constant is inferred to be the same type of the expression.

### Type <a name="type"/>

```
Type = "type" id (TypeAlias|TypeCreation|Enum)
TypeAlias = "as" TypeExpr
TypeCreation = "is" TypeExpr
```

The type declaration can be used to create
aliases to types and new types. An alias is *identical*
to the underlying type, while an new type is only *equivalent*
to it's underlying type. 

Given the following types:

```
type A is int
type B as int
```

The following propositions are true:

 - `A` is *equivalent* to `int` and `B`
 - `A` is not *identical* to `int` or `B`
 - `B` is *identical* to `int`

Aliases cannot be recursive, while new types can be recursive as long
as there's a level of indirection between itself. Given the following types:

```
type A as {.value int, .next &A}
type B is {.value int, .next &B}
type C is {.value int, .next C}
type D is *i8 -> D
type E is *E
type F is &F
```

Type `A` is invalid: it's a recursive alias.
Type `C` is invalid: it's a recursive new type *without*
a level of indirection between itself.
Type `B`, `D`, `E`, `F` are valid: they are a recursive new type with
a level of indirection between itself.

Anu has only a few basic types:

```
    int    i8    i16    i32    i64    bool    nil    void
```

Where `int` is an integer with architecture specific size.
Guaranteed to be the same size as `i32` in amd64.

#### Sums <a name="sums"/>

```
TypeExpr = Map ("|" Map)*
```

Sums are made by using the Or type operator: `|`.
At any given time, the underlying type of a sum is always one (and only one)
of it's possibilities.

 - Two sum types are *equivalent* if they contain *identical* types, regardless of order
 - *Identical* options inside a sum type are treated as a single option
 - Inside a sum type `void` is not considered

```
T | T == T
T | nil == nil | T
(int | *i8) | nil == nil | (*i8 | int)
void | T == T

T | U != T | Z
```

Sums are canonicalized like so:

```
T | T        => T
T | T | U    => T | U
void | T | U => T | U
U | T        => T | U
void | T     => T
```

The only operations allowed with sum types are [`is`](#isoperator),
 [`switch type`](#typeswitch) and [`^`](#)

Example of sums:

```
type Number is i8 | i16 | i32 | i64 | int
type Json   is nil | int | *i8 | *i8->Json | *Json
```

#### Products <a name="products"/>

```
ProductType = "{" Naming ("," Naming)* ","? "}"
Naming = ("." id)? UnaryType
```

Products are made by joining types inside brackets `{}`,
the name of fields can be specified by the `.` operator,
otherwise the default is latin letters in alphabetical order:
`product.a`, `product.b`, ...,`product.z`, `product.aa`, 

 - Two product types are *equivalent* if they have *identical* types layed out in the same order.
 - A product type with a single field is *identical* to that field's type.
 - Inside a product type `nil` is not considered.

```
{int, int} == {.a int, .b int} == {.x int, .y int}
{int} == int
{nil, int} == int
{nil, nil, int} == int

{T, U} != {U, T}
```

Product types are canonicalized like so:

```
{T}         => T
{nil, T}     => T
{nil, nil, T} => T
{nil, U, T}   => {U T}
```

Operations on product types are field access `product.field`, comparison `==`
and destructuring `let a, b = {1, 2}`.

Note: The compiler should warn that single field products
will not keep the field name and will not be able to be accessed
like a product.

#### References <a name="refs"/>

```
UnaryType = TypePrefix* SingleType
TypePrefix = Ref | ArrayType | Optional
Ref = "&"
```

References are constructed with the address-of operator `&`.

 - Two reference types are *equivalent* if they have *identical* base types.

All references point to mutable objects and the object is guarantee
to have a single reference to it. Aliasing references is forbidden,
whenever a reference is copied, the old reference is moved:

```
let a = 1;
let b = &a; # 'a' becomes invalid because it got moved to 'b'
let c = b;  # the contents of 'b' become invalid because it got moved to 'c'
```

The operations allowed on references are dereferencing `@`.
Automatic dereferencing permits references to be used like their base type.

#### Procedure Types <a name="proceduretype"/>

```
ProcType = "proc" TypeArguments TypeExpr
TypeArguments = "[" TypeExprList? "]"
```

Procedure types are built using the `proc` keyword, followed by
the argument types and a single return type.

 - Two procedural types are *equivalent* if they have the same number
 of *identical* parameters layed out in the same order,
 and the return types are *identical*

```
proc[T] T == proc[T] T
proc[T, U] T == proc[T, U] T

proc[T, U] T != proc[U, T] T
proc[] nil != proc[nil] nil
```

#### Array Type <a name="arraytype"/>

```
UnaryType = TypePrefix* SingleType
TypePrefix = Ref | ArrayType | Optional
ArrayType = "*"
```

Array types are made by using the repetition operator `*`.

 - Two array types are *equivalent* if they have *identical* base types

```
*T == *T
*T != *U
```

Arrays have two special fields `length` and `cap`, both of type `int`,
that returns the length and total capacity of the array.

An array of `nil`s should raise a warning, since indexing and setting
operations will not work properly in these cases.

#### Map Type <a name="maptype"/>

```
Map = TypeFactor ("->" TypeFactor)*
```

Maps are built by using the association operator `->`.

 - Two map types are *equivalent* if the keys are of *identical* types,
and the values are of *identical* types

```
*i8 -> int == *i8 -> int
*i8 -> int != int -> *i8
```

The key of a map has to be hashable and comparable,
as such, it cannot be or contain a reference, procedure or map type.

An map with `nil` as the value type should raise a warning, since look-up,
insertion and removing operations will not work properly in these cases. If
the user wants to use the map as a set, he can do the following:

```
type Found is nil
type MySet is *i8 -> Found

proc DoesExist[s:MySet, item:*i8] do
  if s[item] is Found then print["item exists!"]
  else print["item does not exist!"]
```

The type of `s[item]` is `Found | nil`, since `nil` and `Found` are
*equivalent* but not *identical*.

#### Optional <a name="optional"/>

```
UnaryType = TypePrefix* SingleType
TypePrefix = Ref | ArrayType | Optional
Optional = "?"
```

Option types are made by using the maybe operator `?`.

Options are canonicalized into sums:

```
?int  => nil | int
??int => nil | (nil | int)
?&int => nil | &int
&?int => &(nil | int)
?&*int   => &*int | nil
```

#### Enum <a name="enum"/>

```
Enum = "enum" EnumOption ("," EnumOption)* ","?
EnumOption = id ("is" TypeExpr)?
```

Enums can be created with the `enum` keyword, and is equivalent
to multiple type declarations.

```
type YesOrNo enum Yes, No;
```

Can be desugared to:

```
type Yes is nil
type No  is nil
type YesOrNo is Yes | No
```

Enums may contain other information inside it:

```
type Something enum One is {int, int},
                    Two
```

Can be desugared to:

```
type One is {int, int}
type Two is nil
type Something is One | Two
```

## Expr <a name="expr"/>

```
ExprList = Expr ("," Expr)* ","?
Expr = And ("or" And)*
And = Comp ("and" Comp)*
Comp = Sum (compOp Sum)*
compOp = "==" | "!=" | ">" | ">=" | "<" | "<=" | "is"
Sum = Mult (sumOp Mult)*
sumOp = "+" | "-" | ".."
Mult = UnaryPrefix (multOp UnaryPrefix)*
multOp = "*" | "/" | "%"
UnaryPrefix = Prefix* UnarySuffix
UnarySuffix = Factor Suffix*
```

Associativity is always left-to-right,
meaning `a + b + c` will be the same as `(a + b) + c`.
Precedence is defined directly in the grammar,
but can be viewed separatedly in the following table:

| Precedence | Operators                                    |
|:----------:|:--------------------------------------------:|
|     0      | `or`                                         |
|     1      | `and`                                        |
|     2      | `=`, `!=`, `>`, `>=`, `<`, `<=`, `is`        |
|     3      | `+`, `-`, `..`                               |
|     4      | `*`, `/`, `%`                                |
|     5      | `.`, `[]`, `:`, `^`                          |
|     6      | `&`, `@`, `not`, `~`, `$`                    |

Where `[]` means procedure call or indexing, `^` means bubble-up, 
`:` means type annotation and `.` means field access.

### Binary Operators <a name="binaryoperators"/>

 - `or` and `and` are logical or and logical and respectively, they always
operate on two `bool`s and the output is also `bool`.
 - `=` and `!=` are equality and inequality, they work on *comparable* 
types and the output is a `bool`.
 - `>`, `>=`, `<` and `<=` are greater, greater or equals, less,
less or equals respectivelly. They work on *orderable* types and the
 output is a `bool`.
 - `is` is sum identity operator, it takes a sum and a type 
(that must be an option of the sum) and outputs a `bool`.
 - `+` and `-` are the binary sum and subtraction operators, 
they work on integers and the output is of the same type of it's operands.
 - `..` is the array concatenation operator, it takes two arrays of identical types and
outputs a new array with type identical to the operands. 
Since it copies the contents of the arrays, it's subject to move semantics.
 - `*` is the multiply operator. It works on integers and the output is of
 the same type of it's operands.
 - `/` and `%` are the division and remainder operators, they work on numbers
and return an option of the types being operated on: `1 / 1` has type `?int`,
the output of a division or remainder is `nil` when the denominator is zero
(this means division by zero must be explicitly checked).

### Prefix <a name="prefix"/>
```
Prefix = "&" | "@" | "not" | "~" | "$"
```

 - `&` is the address-of operator, it takes any *addressable* expression and returns a reference to that value.
The output type is a reference of the expression type. It's subject to move semantics.
 - `@` is the value-at or dereferencing operator, it takes a reference and returns the base type.
It's subject to move semantics.
 - `not` is the logical operator not, it takes a bool and returns a bool.
 - `~` is the unary minus operator, it takes a single integer and outputs an integer.
 - `$` is the stringify operator, it takes a value of any type and prints a
string (`*i8`) representation of it's values.

### Suffix <a name="suffix"/>

```
Suffix
= PropertyAccess
| CallOrIndex
| TypeAnnot
| TypeReturn
```

#### Call/Index <a name="call"/>

```
CallOrIndex = "[" FieldList "]"
```

If the factor is a procedure, array or map,
or a reference to array or map (automatic dereferencing),
square brackets `[]` can be used to invoke a few operations.

For procedures `[]` performs a procedure call, with each
parameter corresponding to a formal parameter of the procedure.
Since parameters sometimes have names, they may be used to better annotate
a procedure call, if however the procedure is being passed as a value,
named parameters are not available, in this case the compiler must
return an error.

The parameters of a procedure must have type *assignable* to the formal
parameters defined in the procedure declaration, if named parameters
are not used, the order is important, and each parameter must match
the parameters in the declaration in order. If named parameters are used,
each parameter must be of *assignable* type to the respectively named
formal parameter in the procedure declaration. You can't mix and match
ordered and named parameters.

```
proc Add[a:int, b:int] int do a + b;
proc Square[a:int] int do a * a;
proc main do
  begin
    # ordered parameters
    let a = Square[2]
    let b = Add[a, a]

    # named parameters
    let c = Square[a = 2]
    let d = Add[a = c, b = c]
  end
```

The type of a procedure call is the type of the return of the procedure,
the output value of procedure calls are not addressable.

For arrays, `[]` performs either indexing or slicing.
Indexing takes a single parameter of numerical type, and returns
the element at that position, whereas slicing takes two numerical
arguments: the start index and the number of elements to slice.

The output type of an indexing operation is the an option of base type of the array,
(if the array has type `*i8` then indexing it will result in `?i8`)
and the output type for slicing operations are *identical* to the original
array's type. Slicing creates a new copy of a section of the array,
and as such is subject to moving semantics.

```
proc main do
  begin
    let a = \{1, 2, 3, 4}
    let b = a[0] # 1
    set b = a[1] # 2
    let c = a[0, 2] # \{1, 2}
    let d = a[0, a.length] # \{1, 2, 3, 4}
  end
```

For maps, `[]` performs a map look-up, it takes an expression
of type *identical* to the map's key type, and the output
is an option of the map's value type. (if a map has type `*i8 -> int`
performing a map lookup will result in `?int`).

```
proc main do
  begin
    let a = \{"a" -> 0, "b" -> 1}
    let b :?int = a["a"]
    if b is nil then fatal["key not found"]
  end
```

#### Property Access <a name="propertyaccess"/>

```
PropertyAccess = "." id
```

If the type is a product, map or array, 
or reference to a product, map or array (automatic dereferencing),
`.` can be used to access a field inside it.

For products, the field must exist inside it, if the product has named
fields, the names must match those given by the user, however, if
fields are unamed, they default to one or more letters in alphabetical order 
(`product.a`, `product.b`,... , `product.z`, `product.aa`, ...).

```
proc main do
  begin
    let a = {1, 2, 3}
    let b = a.a # 1
    set b = a.b # 2
    set b = a.c # 3
  end
```

For arrays and maps, there are only two fields `length` and `cap`, 
that returns an `int` representing respectivelly the length and capacity
of the array or map.

#### Bubble Up <a name="bubbleup"/>

```
TypeReturn = "^" TypeExpr
```

If an expression is of a sum type, the `^` operator can be used to
perform an early return of unwanted values. It returns any value
that is different from the type specified and outputs a value of that
type. The type specified must be an option inside the sum.

```
proc PrintFile[file:*i8] ?IO::Error do
  begin
    let a = IO::Open[file, IO::ReadOnly]^&File
    let contents = IO::ReadAll[a]^*i8
    IO::Print[contents]
    return nil
  end
```

The above code can be desugared into:

```
proc PrintFile[file:*i8] ?IO::Error do
  begin
    let a = switch type IO::Open[file, IO::ReadOnly] as v
            case IO:Error then return v
            case IO::File then v
    let contents = switch type IO::ReadAll[a] as v
                   case IO:Error then return v
                   case *i8 then v
    IO::Print[contents]
    return nil
  end
```

#### Type Annotation <a name="typeannot"/>

```
TypeAnnot = ":" TypeExpr
```

The `:` is used for type annotation or casting. If the expression
has type *identical* to the annotated type, then no operation is performed,
if the type is not *identical* but is *castable*, then the value is casted
to the annotated type. If the type is neither *identical* nor *castable*
then it's an invalid type annotation.

## Factor <a name="factor"/>

```
Factor
= Name
| int | bool | rune | string | nil
| ArrayMapLit
| ProductLit
| For
| Switch
| If
| NestedExpr
| Block
| EarlyReturn
| Set
| Let
| New
```

Factors are the operands of expressions,
though they may contain subexpressions themselves.

### NestedExpr <a name="nestedexpr"/>
```
NestedExpr = "(" Expr ")"
```

Parenthesis may be used to nest expressions inside one another,
they may also be used to specify precedence order: `a + (b + c)`
evaluates `b + c` first.

It is also important to disambiguate dangling-elses.

### Block <a name="block"/>
```
Block = "begin" ExprSemicolon* "end"
ExprSemicolon = Expr ";"?
```

A block expression is a way to perform operations
and discard their values, the type of a block expression
is the type of the last expression or `nil`.

```
proc F[] int do
  begin
    1
  end
```

Expressions inside a block are evaluated top-down, and blocks
create a separated lexical scope.

### Product Literal <a name="productliteral"/>

```
ProductLit = ComplexLitBody
ComplexLitBody = "{" (":" TypeExpr)? FieldList? "}"
FieldList = Field ("," Field)* ","?
Field = Expr ("=" Expr)?
```

A product literal alocates a product type and sets it's values.
The literal can be inferred or explicitly typed, named or unamed.
It's an error to mix and match named and unamed fields.

When explicitly typed, each field must have type *identical*
to the field specified in the type, if the fields are unamed,
the fields must be present in order, if names are used,
the type must match the respectively named field in the type.

Example of valid explicitly typed product literals:

```
const a = {:{int, int} 1, 2}
const b = {:{int, *i8, int} 1, "a", 2}
const c = {:{.a int, .b *i8} b = "second", a = 1}
```

Example of invalid explicitly typed product literals:

```
const a = {:{int, int} 1}
const b = {:{int, *i8, int} "a", 1, 2}
const c = {:{.a int, .b *i8} c = "second", a = 1}
```

When inferred, the resulting product has type equal to the
product of each field concatenated together, even with names present,
the order matters.

Example of types inferred from product literals:

```
const a :{int, int} = {1, 2}
const b :{*i8, int} = {"a", 2}
const c :{.b int, .a int} = {b = 2, a = 1}
```

### Array/Map Literal <a name="arraymapliteral"/>

```
ArrayMapLit = "\" ComplexLitBody
```

An array or map literal differ by the use of pairs in the literal body,
pairs are made using the assignment operator `=`. Both array and map
literals can be type annotated or inferred, when type annotated,
each expression is checked to match the annotation, while inferrence occurrs
differently for maps and arrays.

For array literals, the inferred type is an array of the sum of the
type of all expressions inside the body.
A literal `\{1, "1", true}` has type `*(int|*i8|bool)`.

For map literals, the inferred type is a map with key type equal to the
sum of the type of all key expressions, and a value type equal to the
sum of the type of all value expressions. In other words, a literal
`\{"a" = 1, 1 = "a", 1 = 1}` has type `*i8|int -> *i8|int`.

### For <a name="for"/>

```
For = "for" (Conditional | Iterative | Range)
```

`for` is the loop construct in anu, it has 3 forms,
conditional, iterative and ranged.

The type of the `for` expression is always an array of the type
of it's body.
`for true do 1;`,
`for each item in array do 1;` and
`for range 0 to 10 do 1;` all have type `*int`

#### Conditional <a name="conditional"/>
```
Conditional = Expr "do" Expr
```

The conditional form is the simplest form of looping,
it takes the form of:

```
for <cond> do <expr>
```

Where `<cond>` is an expression of type `bool`. It executes the `<expr>`
until `<cond>` equals `false`.

#### Iterative <a name="iterative"/>
```
Iterative = "each" id ("," id)? "in" Expr "do" Expr
```

The iterative form is used to iterate over items of an array or map,
a `for each` expression will loop until all items of an array or map are
evaluated (or until an early return occurs). It takes the form of:

```
for each <id> in <collection> do <expr>
```

Where `<collection>` is an expression with array or map type, it
evaluates `<expr>` until all items are consumed. The `<id>` name
is valid inside `<expr>`. A second identifier might be used
to represent other values. Both identifiers are immutable.

Both identifiers are immutable. Modifying the collection should not result
in the value of the identifiers being modified, and should not result in
runtime errors.

If the collection is an array, the type of `<id>` is the base type of
the array, and if a second identifier is used (`for each <id>, <id> in ...`)
The second identifier represents the index of the item, and has type `int`.

If the collection is a map, the type of `<id>` is the key type of the map,
and if a second identifier is used, it's type will be the 
value type of the map.

Given a `for` in the form of: `for each <first>, <second> in <collection> do ...`
It can be summarized in the following table

| collection | first    | second      |
|:----------:|:--------:|:-----------:|
| array `*T` | item `T` | index `int` |
| map `K->V` | key `K`  | value `V`   |

#### Range <a name="range"/>
```
Range = "range" Expr "to" Expr ("as" id)? "do" Expr
```

The `for range` loop iterates in a range of numbers, incrementally
or decrementally, and evaluates the expression once for each number.
It takes the form:

```
for range <start> to <end> as <id> do <expr>
```

Where `as <id>` may be omitted. Both `<start>` and `<end>`
must be numerical. The type of `<id>`
is the same type as `<start>` and `<end>`.
`<end>` is exclusive and `<id>` is immutable.

If `<start>` is bigger than `<end>`, the value
of `<id>` starts equal to `<start>`, goes down by 1 until it
is equal to `<end>`, when it stops iterating.

If `<start>` is smaller than `<end>`,
the value of `<id>` starts equal to `<start>`, goes up by 1
until it is equal to `<end>`, when it stops iterating.

If `<start>` is equal `<end>` the loop will not evaluate `<expr>`

### Switch <a name="switch"/>
```
Switch = "switch" (TypeSwitch | ValueSwitch)
```

`switch` expressions are like `if` expressions
but they are specialized to constant values. There are
two kinds of switches, a type switch and a value switch,
one operates on values, the other operates on types.

```
switch <expr>
case <const_expr0>, ..., <const_exprN> then <expr1>
case <const_exprN+1>, ..., <const_exprN+M> then <expr2>
...
default <exprN>
```

The output type of value and type switches work the same way,
similarly to `if`s, the output type of a `switch` expression is the sum
of the types of all `then` expressions. The type of the previous
example would be:
`typeof(<expr1>) | typeof(<expr2>) | ... | typeof(<exprN>)`.

If the default case is not specified, the type of the switch expression
is the sum of the types of all `then` expressions together with `nil`.
The type of the following expression is: 
`typeof(<expr1>) | ... | typeof(<exprN>) | nil`

```
switch <expr>
case <const_expr> then <expr1>
...
case <const_expr> then <exprN>
```

#### Value Switch <a name="valueswitch"/>
```
ValueSwitch = Expr ValueCase* Default?
ValueCase = "case" ExprList "then" Expr
Default = "default" Expr
```

A value switch takes the following form:

```
switch <expr>
case <const_expr0>, ..., <const_exprN> then <expr1>
case <const_exprN+1>, ..., <const_exprN+M> then <expr2>
...
default <exprN>
```

Where `<const_expr>` is a expression with a static value known at
compile time. Evaluation of a switch expression can be thought that 
it compares the `<expr>` successfully with each `<const_expr>` and
if it's equal, it jumps to the `then` part.

With this evaluation strategy, it's not an error to have two or more
equal cases in a switch, but compilers should warn if that happens.

#### Type Switch <a name="typeswitch"/>

```
TypeSwitch = "type" Expr ("as" id) TypeCase* Default?
TypeCase = "case" TypeExprList "then" Expr
```

The type switch is similar to the value switch but it operates on
types, types in anu are always static. The type switch may also create an
*immutable* alias of the value being switched on.
In the following example `as <id>` may be omitted.

```
switch type <expr> as <id>
case <type_expr0>, ..., <type_exprN> then <expr1>
case <type_exprN+1>, ..., <type_exprN+M> then <expr2>
...
default <exprN>
```

The type of `<id>` is dependent on the control flow.
It's type in the `then` is the sum of all types present in the `case`.
In `<expr1>` the type of `<id>` is `<type_expr0> | ... | <type_exprN>`.
In the `default` branch, the type of `<id>` is identical to the type of
`<expr>`.

If the `<expr>` is a mutable variable and is modified inside a `case`,
the value of `<id>` will not change, it will remain the same as it was on
the beginning of the switch.

The output type of the `switch type` expression follows the same rules
as the value switch.

### If <a name="if"/>
```
If = "if" Expr "then" Expr ElseIf* Else?
ElseIf = "elseif" Expr "then" Expr
Else = "else" Expr
```

The `if <cond> then <expr>` construct evaluates `<expr>`
if the value of `<cond>` is `true`, otherwise,
execution procedes to the next `elseif` condition (if present),
if none of the `elseif` branches are entered, the `else` branch
is entered (if present).

The output type of an `if` expression is the sum of the types
of each expression body, that is, given the following structure,
the type of the `if` expression becomes
`typeof(<expr1>) | typeof(<expr2>) | ... | typeof(<exprN>)`

```
if <cond0> then <expr1>
elseif <cond1> then <expr2>
...
else <exprN>
```

If the `else` branch is not present, the output type is the
sum of the types of each expression body, together with `nil`.
In other words, given the following structure, the type of the 
`if` expression becomes
`typeof(<expr1>) | typeof(<expr2>) | ... | typeof(<exprN>) | nil`

```
if <cond0> then <expr1>
elseif <cond1> then <expr2>
...
elseif <cond1> then <exprN>
```

Due to sum type canonicalization, it's possible to write an
`if` expression like the following, where the output type is only
`int`.

```
proc factorial[n:int] int do
  if n == 0 then 1
  else n * factorial[n-1]
```

### EarlyReturn <a name="earlyreturn"/>
```
EarlyReturn = "return" Expr
```

A return expression performs a (possibly early) return
from a procedure, the type of the expression must be
*assignable* to the return type of the procedure.
The output type of this expression is `void`.

Note that the following procedures are invalid since
return has type `void`, which is not assignable to `int`:

```
proc F[] int do
  return 1;

proc G[] int do
  begin
    return 1;
  end
```

### Let <a name="let"/>
```
Let = "let" LetDeclList ("in" Expr)?
LetDeclList = LetDecl ("," LetDecl)* ","?
LetDecl = VarList "=" Expr
VarList = Annotated ("," Annotated)* ","?
Annotated = id TypeAnnot?
```

`let` is the only way to declare variables inside a procedure,
it may be used to declare more than one variable at once,
and may be used to create it's own scope.

The output of a `let` expression is the value of the expression after
`in`, if present, if not present, it's value is `nil`.

In the following procedure, `a` is available when `b` is being
defined, and the output of the procedure is `a * b`. The variables
`a` and `b` are only accessible inside the `in` expression.

```
proc F[] int do
  let a = 3,
      b = 2 + a in a * b;
```

If the `in` is not present, the variable is declared in the
scope the `let` expression is present in. The following procedure
is equivalent to the above, but doesn't use `in`.

```
proc F[] int do
  begin
    let a = 3
    let b = 2 + a
    a * b
  end
```

Observe that two variables in the same scope cannot have the same
name, the following procedure is in error:

```
proc F[] int do
  let a = 3,
      a = 2 + a in 3 * a
  #   ^ error: variable `a` already declared in this scope
```

However, since `in` creates a new scope, the following is fine:

```
proc F[] int do
  begin
    let a = 3
    let a = a + 2 in 3 * a;
    #   ^ shadows the previous `a` in this inner scope
  end
```

Destructuring products works the same in `let` as in `set`,
if the right side after `=` is a product, the identifiers on the
left side are declared and set with the respective field from
the product, in order.

```
proc F[] int do
  let a, b = {1, 2} in a + b;
```

Observe that since the right side evaluates first,
the values on the left side are not available until the next declaration
or until the `in` expression.

Type annotation works the same as with `const`ants, if the
left side is annotated, the right side must be *assignable*
to this type.

It's possible to combine both destructuring and annotation.

```
proc F[] ?int do
  let a:?int, b:?int = {1, 2}
     in a^int * b^int;
```

### Set <a name="set"/>
```
Set = "set" ExprList assignOp Expr
assignOp = "=" | "-=" | "+=" | "*=" | "..=" | "<->" | "remove"
```

All mutation in anu uses the `set` expression,
the expression on the right side must be *assignable* to the
expression on the left, however, the `<->` operator requires
that both sides are *assignable*.
There are multiple operators available to a `set` expression,
in all cases, the right side always evaluates before the left side.

Assignment requires that at least the left side is *mutable*,
requiring both sides to be *mutable* on a swap `<->`.
The following expressions can be the target of an assignment.

 - Mutable variable, eg. `set a = 1`
 - Indexing of a mutable variable, eg. `set a[0] = 1`
 - Look-up of a mutable variable, eg. `set a["key"] = 1`
 - Field access of a mutable variable, eg. `set a.a = 1`
 - A dereferencing operation, eg. `set @a = 1`

`=` copies the value from the right side to the left side,
if the right side is a product, the left side may contain more than
one expression, where the tuple is destructured and each value is assigned
in order to the left operands.

```
proc main do
  begin
    let a = 1;
    set a = 2; # a is now 2

    let c = 0
    let b = {1, 2}

    set a, c = b # destructures 'b' and assigns 'a' and 'c'
  end
```

Note that order matters:

```
proc main do
  begin
    let a = 0, b = {1, 2};
    set a, a = b # a is now equals 2
  end
```

The output of an `set ... = ...` assignment is the value of the right side.

The expression assign operators `+=`, `-=` and `*=`
works with numeric values by updating the left side after the operation.
They can be desugared:

 - `set a += 1` is equivalent to `set a = a + 1`
 - `set a -= 1` is equivalent to `set a = a - 1`
 - `set a *= 1` is equivalent to `set a = a * 1`

The `..=` operator appends an array to the end of the other.
The arrays must be of *identical* types, the output is also *identical*.
`set a ..= \{1, 2, 3}` is equivalent to `set a = a .. \{1, 2, 3}`, but
`a` is updated in-place, that is: if `a` has enough capacity to hold
all items of the array in the right side, it will not allocate a new
array.

Since `..` copies items from one array to the other,
`..=` is also subject to moving semantics,
specially `set a ..= a` will not work if the type of `a` contains
references.

The `set ... remove ...` expression removes an item from a map and
returns the item. `set a remove "key"` removes the value corresponding
with the key `"key"` from the map `a` and returns it's value. Looking up
a key in a map and removing that key should return the same item, if present,
otherwise it should return `nil`. Given a map of type `*i8 -> int`, the
output type of a remove expression on this map should be `?int`.
Removing an item from a map will decrease the value of `length` but not of `cap`.

```
proc main do
  begin
    let a = \{"key" = 1, "abracadabra" = 2}
    let b : ?int = set a remove "key" # 'b' is equal to 1
  end
```

The swap operator `<->` swaps the contents of two *assignable* expressions.
The right side still evaluates first, and both sides evaluate only once.

```
set a <-> b
```

This is specially important when swapping items of arrays or maps,
since using destructuring would invalidate the whole object, not only
the item. `set a, b = {b, a}` will not work if you substitute `a` or `b`
for an indexing or look-up expression that contains references.

Note that `/=` and `%=` are missing for good cause, they output a type
different from it's operands: `1 / 2` is of type `?int` not `int`.

If the left side of a `set` expression is an array indexing, the output 
will be an option indicating whether the indexing was sucessful,
in the following example, `b` is `nil` when `a[0]` is out of bounds:

```
let b :?int = set a[0] = 1;
```

If the left side contains multiple expressions and one of them is
an array indexing, the output will still be a simple option,
but the compiler must warn the user. The compiler should also warn
the user if he is discarding the option.

To reset the `length` of an array, you can set it's value to zero,
its only possible to assign this field with zero, and nothing else.

```
set array.length = 0
```

### New <a name="new"/>
```
New = "new" TypeAnnot "[" FieldList? "]"
```

The `new` construct allows the user to prealocate an array or map
by setting it's `cap` and `length`, or to alocate a type with the
hability to check for Out of Memory conditions. `new` always
returns an optional, since it may fail to alocate.

For arrays, the user may set `cap` or `length` with a number,
if `length` is present, it's necessary to specify an `init` value,
that will be used to initialize every item in the array, 
this value may not be a reference (since it would create multiple copies).
If both `cap` and `length` are present, `length` must be smaller or equals
to `cap`. If `length` is not present, it defaults to `0`. The `init`
must be a type *castable* to the base type of the array.

```
proc main do
  begin
    # if making arrays by 'length', you must specify the 'init'
    let buff0: ?*i8 = new:*i8[length = 512, init = 0]

    # however, if making by 'cap', you can omit the 'init',
    # since the length will default to 0
    let buff1: ?*i8 = new:*i8[cap = 512]

    # if you specify both, 'cap' must be bigger than 'length'
    # and 'init' must be set
    let buff2: ?*i8 = new:*i8[cap = 512, length = 16, init = 0]
  end
```

For maps, only `cap` may be set, this prealocates the map
but doesn't insert any values.

```
proc main do
  begin
    let mymap: ?(*i8->int) = new:*i8->int[cap = 512]
  end
```

For other types, `new` serves only as a checked alocation,
first it tries to alocate a region of memory, then sets
with the value you passed it. The value must be *castable*
to the type specified in `new`

```
type BigDataStructure is {
  int, int, int, int, int, int
}

proc main do
  begin
    let a: ?BigDataStructure = new:BigDataStructure[{1, 1, 1, 1, 1, 1}]
  end
```

Using `new` with scalar values should return a warning,
since scalar values are likely to be stack alocated.

# Semantics <a name="semantics"/>

## Type Canonicalization <a name="typecanonicalization"/>

Types must be canonicalized before any comparisons can be made with them.
The rules for canonicalization are:

 - *Identical* options inside a sum type are canonicalized into a single option
 - Inside a sum type `void` is discarded during canonicalization
 - A product type with a single field is canonicalized to that field's type
 - Inside a product type `nil` is discarded during canonicalization.
 - An option is canonicalized into a sum `?T` becomes `nil | T`

## Type Identity <a name="typeidentity"/>

Before two types can be compared for identity, they must first
be canonicalized.

Types created with `type ... is ...` are referred to as "new types",
types created with `type ... as ...` are referred as "aliases",
a type is considered anonymous if it has not been assigned a name.

Note: `enum` always creates new types, not aliases.

- New types are unique, two differently named types are never *identical*.
- New types declared in different modules are never *identical*.
- Aliases and anonymous types are *identical* if they're *equivalent*.

## Type Equivalence <a name="typeequivalence"/>

Before two types can be compared for equivalency, they must first
be canonicalized.

If they're *identical* they are also *equivalent*.
Otherwise types are equivalent if they're structurally equivalent:

 - Two product types are *equivalent* if they have *identical* types
layed out in the same order.
 - Two sum types are *equivalent* if they contain *identical* types,
regardless of order
 - Two procedural types are *equivalent* if they have the same number
 of *identical* parameters layed out in the same order,
 and the return types are *identical*
 - Two map types are *equivalent* if the keys are of *identical* types,
and the values are of *identical* types
 - Two array types are *equivalent* if they have *identical* base types
 - Two reference types are *equivalent* if they have *identical* base types.

## Assignable <a name="assignable"/>

A type `T` is assignable to a type `U` if:

 - `T` is *equivalent* to `U`
 - `U` is a sum and `T` is *identical* to one of the options

## Castable <a name="castable"/>

A type `T` is castable to a type `U` if one of the following
is true:

 - both `T` and `U` are numeric types
 - `T` is numeric and `U` is a `bool`
 - `U` is a sum and `T` is *identical* to one of the options
 - `T` is *equivalent* to `U`

## Immutable/Mutable <a name="immutablemutable"/>

Most expressions are immutable, mutable expressions
are only uses of mutable variables and dereferencing operations.
In other words, an object is mutable only if declared with the
`let` construct, and references to this object may modify it.

The contents of an mutable object may be accessed and are themselves
mutable too. This is valid for field access in products, indexing
in arrays and look-ups in maps. In other words, if `a` is mutable,
then `a[1]`, `a["a"]` and `a.a` are also mutable

All other things: procedure arguments, procedure returns,
literals, aliases etc, are immutable.

## Addressable <a name="addressable"/>

An expression is addressable if it's a mutable variable.
In the following code, only `c` is addressable.

```
proc P[a:int, b:int] int do
  begin
    let c = a

    for each item in \{1, 2, 3} do 1+1;

    for range 0 to 10 as i do i+1

    switch type 1:?int as v
    case int do 1
    default nil

    return 1
  end
```

## Moving semantics <a name="movingsemantics"/>

Moving semantics guarantee that all references in Anu are the sole owner
of the objects they point to.
Whenever a reference is copied it's ownership is moved and the contents
of it's previous container becomes invalid. Whenever a variable has it's
address taken, the variable becomes invalid.

```
proc main do
  begin
    let a = 0
    #        v moves 'a'
    let b = &a

    #       v error: use of moved variable
    let c = a + 1;
  end
```

For containers, it is possible to restore the validity of their
references:

```
proc main do
  begin
    let a = 0
    #        v moves 'a'
    let b = &a

    # v 'F' takes the ownership of 'b's references
    F[b]

    let c = 1
    #        v moves 'c'
    set b = &c
    #   ^ 'b' becomes valid again
  end

proc F[&int] do nil
```

If any branch of a procedure moves a reference, after the branch the
reference is invalid:

```
proc main do
  begin
    let a = 0
    if true then F[&a]

    #       v error: use of possibly moved variable
    let b = a + 1;
  end

proc F[&int] do nil;
```

For loops, if any variable is invalid at the end of the loop expression
it's also invalid in the beginning of the loop:

```
proc main do
  begin
    let a = 0
    #             v error: variable was moved on previous loop iteration
    for true do F[&a]
  end

proc F[&int] do nil;
```

References are also moved when creating array, map or
product literals and passing parameters to procedures.


```
proc main do
  begin
    let a = 0, b = 1
    let z = \{&a, &b}
    #          ^   ^ both are moved
  end
```

If *any reference* from inside an array or
map is moved, the whole array/map is invalid until a full copy or reset,
since it is impossible in the general case to determine whether arbitrary
indexes inside an array or map have been restored.

```
proc main do
  begin
    let a = 0, b = 1, c = 2
    let z = \{&a, &b}
    F[z[0]]
    #   ^ moves reference from inside 'z'

    set z[0] = &c
    #        ^ error: setting item of array with moved references

    let d = 3
    set z = \{&d}
    #   ^ now 'z' is valid again
  end

proc F[&int] do nil;
```

The swap operator `<->` is specially useful in those cases, you can swap
a reference from inside an array/map with another reference or value,
and keep the validity of the whole collection.

```
proc main do
  begin
    let a = 0, b = 1
    let z = \{:*?&int &a, &b}

    let c : ?&int = nil
    swap z[0] <-> c;
    # now 'c' contains the reference you want to work with
    # and 'z[0]' contains 'nil'
    
    F[c]
    # ^ moves the contents of 'c'
    # but 'z' is still valid
  end

proc F[?&int] do nil;
```

The `..` and `..=` operators are also subject to moving semantics
since they create a copy of the array:

```
proc main do
  begin
    let a = 0, aa = 1, aaa = 2
    #                 v   v    v they are moved
    let b :*&int = \{&a, &aa, &aaa}

    #       v the contents of 'b' are moved
    set b = b .. b;
    #            ^ error: use of container with moved references

    set b ..= b;
    #   ^ error: use of container with moved references
  end
```

In a `switch` expresion, aliasing moves the variable or the contents
of the container.

```
proc main do
  begin
    let a = 0
    let z :int | &int = &a

    switch type z as x
    #                ^ moves the contents of 'z'
    case &int then F[x]
    #                ^ moves the contents of 'x'
    case int then nil

    let abc = z
    #         ^ error: use of container with moved references
  end

proc F[&int] do nil;
```

For `for each ... in ...` loops, aliasing also borrows the variable
or the contents of the container. But if the alias is moved, the whole
collection becomes invalid. Aliasing shadows the collection being
iterated on.

```
proc main do
  begin
    let a = 0, b = 1, c = 2
    let z = \{&a, &b, &c} 

    #                v error: use of container with moved references
    for each item in z do
      F[item]
  end

proc F[&int] do nil;
```

The following is a valid way to operate on references inside a `for`:

```
proc main do
  begin
    let a = 0, b = 1, c = 2
    let z = \{:*?&int &a, &b, &c}

    # frees all objects referenced by 'z'
    for range 0 to z.length as index do
      begin
        let v :?&int = nil
        set z[index] <-> v
        F[v]
      end

    # 'z' is still valid here (it's full of nils)
  end

proc F[?&int] do nil;
```

Returning a reference from a procedure also returns it's ownership,
in the following procedure, `F` essentially borrows `a`, and returns it.

```
proc F[a:&int] &int do a
```

## Freeing Semantics <a name="freeingsemantics"/>

Objects are freed as soon as possible. An object may be freed when:

 - You change the value of a reference
 - You change the value of an object that contains references
 - A reference value is no longer used in or returned from a scope that has it's ownership
 - A reference value is discarded

Examples: 

Changing the value of a reference:

```
proc F do
  begin
    let a = 1;
    let b :?&int = &a
    set b = nil # `a` is freed
  end
```

Changing the value of an object that contains references:

```
proc F do
  begin
    let a = 1;
    let b = 2;
    let c :?{&int, &int} = {&a, &b}
    set c = nil # `a` and `b` are freed
  end
```

Slicing is subject to moving semantics, when slicing an array that contains
references, the remaining objects not contained in the slice are freed,
since they are no longer used in the scope (they are not reachable).

```
proc main do
  begin
    let myInt = 0, mySecondInt = 1
    let a = \{&myInt, &mySecondInt}

    a[0, 1] # this frees 'mySecondInt' internally
    # 'a' is invalid here
  end
```

Objects are freed when they're no longer being used in that
branch path. In the following example, the array `a` is freed
once in each possible `if` branch, even before the end of the block.

```
proc main do
  begin
    let a = new:*int[cap = 1024]
    let b = if true then a[0] # 'a' is freed here
            else a[1]         # and also here
  end
```

When branching, if any branch takes ownership and doesn't return it,
the object may be safely freed earlier.

```
proc main do
  begin
    let a = 1;

    if true then a # here 'a' is not moved but since it's invalid
                   # after the branch, and thus not used, it is freed
    else let b = &a in @b + 1 # 'a' is moved to 'b', and 'b'
                              # is no longer used in the scope
                              # so it's freed

    # 'a' is invalid here
  end
```

If an object is returned from a procedure,
it's not freed by the procedure.

```
proc F[] &int do
  let a = 1 in &a  # 'a' essentially escapes this procedure
                   # and is not freed here
```

An object used inside a loop is being used for the remaining of the loop,
unless it's ownership is moved.

```
proc F do
  begin
    let a = 1
    for range 0 to 10 do
      set a += 1
    # 'a' is freed only here
  end
```

However:

```
proc G do
  begin
    let a = 1
    let b = &a
    for range 0 to 10 do
      begin
        F[b] # frees either 'a' or 'c'
        let c = 1
        set b = &c # 'c' is moved to 'b'
      end

    # 'b' is still valid
    # and will be freed after the for loop
  end

# 'a' will be freed inside this function
proc F[&int] do nil;
```

## Scopes <a name="scopes"/>

In the following examples, the arrows indicate the range of each scope
and are accompained by a number that indicates the level of nesting
of each scope.

There are two implicit scopes created in any anu program: the universe
scope and the global module scope. They are nested like so:

```
------------------------------------
| universe                         |
| ---------- ---------- ---------- |
| | module | | module | | module | |
| ---------- ---------- ---------- |
------------------------------------
```

Where each `module` represents the global module scope of a separate module.
Built-ins are declared in the `universe` scope and may be shadowed by
declarations in each nested scope.

All Header imports create names in the global space of each module.
While exports can only refer to non-imported names living in the global
space.

Here `F` is declared in the global scope of the module, while `a` and `b`
are declared in a separate argument scope. Finally the `do ...` lives
in its own scope too.

```
--0--v
proc F[a:int, b:int] do ...
      ^-----1------^    ^--2---
```

The two following `for` expressions may create scopes that are nested
as described, note that the `do ...` can always shadow the previously
declared variables.

```
                       v--0--v
for each index, item in array do ...
         ^----1----^             ^--2---

for range 0 to 10 as  i  do ...
         ^---0---^  ^-1-^   ^--2--
```

The `switch type` behaves similarly, where the `as` keyword
creates a new scope with a single variable that can be shadowed inside
the `case`s.

```
---------0--v    v-1-v  v---2---
switch type a as   x    case ... then ...
```

The `let` construct has a more complicated scope rule, in the following
example, all variables declared inside the `let ... in` live in the
same scope, and are evaluated in order, as they appear in text.

```
let a = 0  in ...
   ^--0--^    ^--1---

let a = 0, b = 1, c = 2 in ...
   ^---------0---------^   ^--1---
```

While if the `let` has no `in`, the variables are declared in the
surrouding scope:

```
let a = 0, b = 1
--0------------^
```

Finally, a block can create a scope without declaring any variable:

```
begin  ...  end
     ^--0--^
```

# Misc <a name="misc"/>

## Full Grammar <a name="fullgrammar"/>

```
Anu {
Module = Header Body

Header = (ModuleRelations ";"?)*
Body = (Symbol ";"?)*

ModuleRelations = Import | Export | FromImport

Import = "import" AliasList
FromImport = "from" id "import" AliasList
Export = "export" ("all" | AliasList)
AliasList = Alias ("," Alias)* ","?
Alias = id ("as" id)?

Symbol = Constant | Procedure | Type

Procedure = "proc" id TypeAnnot? Signature "do" Expr
Constant = "const" id TypeAnnot? "=" Expr
Type = "type" id (TypeAlias|TypeCreation|Enum)

Signature = (Arguments (Return)?)?
Arguments = "[" DeclList? "]"
Return = TypeExpr
DeclList = Decl ("," Decl)* ","?
Decl = id TypeAnnot?

TypeAnnot = ":" TypeExpr

TypeAlias = "as" TypeExpr
TypeCreation = "is" TypeExpr
Enum = "enum" EnumOption ("," EnumOption)* ","?
EnumOption = id ("is" TypeExpr)?

TypeExpr = Map ("|" Map)*
Map = TypeFactor ("->" TypeFactor)*
TypeFactor = TypePrefix* SingleType
SingleType = Name | NestedType | ProcType | ProductType | nil
TypePrefix = Ref | ArrayType | Optional
ArrayType = "*"
Optional = "?"
Ref = "&"
ProductType = "{" Naming ("," Naming)* ","? "}"
Naming = ("." id)? UnaryType
NestedType = "(" TypeExpr ")"
ProcType = "proc" TypeArguments TypeExpr
TypeArguments = "[" TypeExprList? "]"

TypeExprList = TypeExpr ("," TypeExpr)* ","?

Name = id ("::" id)?

ExprList = Expr ("," Expr)* ","?
Expr = And ("or" And)*
And = Comp ("and" Comp)*
Comp = Sum (compOp Sum)*
compOp = "==" | "!=" | ">"~("="|"-") | ">=" | "<"~("="|"-") | "<=" | "is"
Sum = Mult (sumOp Mult)*
sumOp = "+" | "-" | ".."
Mult = UnaryPrefix (multOp UnaryPrefix)*
multOp = "*" | "/" | "%"
UnaryPrefix = Prefix* UnarySuffix
UnarySuffix = Factor Suffix*
Prefix = "&" | "@" | "not" | "~" | "$"
Suffix
= PropertyAccess
| CallOrIndex
| TypeAnnot
| TypeReturn

PropertyAccess = "." id
TypeReturn = "^" TypeExpr
CallOrIndex = "[" FieldList "]"

Factor
= Name
| int | bool | rune | string | nil
| ArrayMapLit
| ProductLit
| For
| Switch
| If
| NestedExpr
| Block
| EarlyReturn
| Set
| Let
| New

NestedExpr = "(" Expr ")"

Block = "begin" ExprSemicolon* "end"
ExprSemicolon = Expr ";"?

ArrayMapLit = "\" ComplexLitBody
ProductLit = ComplexLitBody
ComplexLitBody = "{" (":" TypeExpr)? FieldList? "}"

FieldList = Field ("," Field)* ","?
Field = Expr ("=" Expr)?

For = "for" (Conditional | Iterative | Range)
Conditional = Expr "do" Expr
Iterative = "each" id ("," id)? "in" Expr "do" Expr
Range = "range" Expr "to" Expr ("as" id)? "do" Expr

Switch = "switch" (TypeSwitch | ValueSwitch)
ValueSwitch = Expr ValueCase* Default?
TypeSwitch = "type" Expr ("as" id) TypeCase* Default?
ValueCase = "case" ExprList "then" Expr
TypeCase = "case" TypeExprList "then" Expr
Default = "default" Expr

If = "if" Expr "then" Expr ElseIf* Else?
ElseIf = "elseif" Expr "then" Expr
Else = "else" Expr

EarlyReturn = "return" Expr

Let = "let" LetDeclList ("in" Expr)?
Set = "set" ExprList assignOp Expr
LetDeclList = LetDecl ("," LetDecl)* ","?
LetDecl = VarList "=" Expr

VarList = Annotated ("," Annotated)* ","?
Annotated = id TypeAnnot?

New = "new" TypeAnnot "[" FieldList? "]"

keyword = "import" | "from"    | "export" | "proc" | "const"  | "type"   |
          "begin"  | "end"     | "is"     | "or"   | "and"    | "not"    |
          "do"     | "for"     | "if"     | "each" | "in"     | "switch" |
          "case"   | "default" | "elseif" | "else" | "return" | "let"    |
          "set"    | "as"      | "to"     | "then" | "range"  | "nil"    |
          "new"    | "remove"  | "void"   | "true" | "false"

assignOp = "=" | "-=" | "+=" | "*=" | "..=" | "<->" | "remove"

id = ~(keyword ~letter) letter alnum*
nil = "nil"
int = digit+
bool = "true" | "false"
rune = "\'" insideRune* "\'"
insideRune = (~"\'" any | "\\\'")
string = "\"" insideStr* "\""
insideStr = (~"\"" any | "\\\"")
comment = "#" (~lineTerminator any)*
space := lineTerminator | whitespace | comment
lineTerminator = "\n"
whitespace = " " | "\t"
}
```

## Builtin Modules <a name="builtinmodules"/>

Every anu program, regardless of the folder it's present, has 
access to a few builtin modules that are part of the standard
library. These modules are special in the sense that they can be
shadowed by other modules in the current folder.

### os

The `os` module has basic syscalls for dealing with I/O. These include:
`open`, `close`, `write` and `read` procedures, as well as `stdin`, `stdout`
and `stderr` constants for file descriptors and the `fd` type.

## Future <a name="future"/>

 - Bitwise operators
 - `with` expression for non-destructive mutation of products
 - Some form of reflection that returns stack trace information
 - Make maps comparable and hashable

Maybe never:
 - Rank 1 polymorphism with constraints and inference
 - `target` construct for build tags aroung symbols
 - `asm` procedures and a clear, well defined ABI

# Examples <a name="examples"/>

## Rock, Paper, Scissors

```
proc Winner[first:*i8, second:*i8] ?*i8 do
  let rules = \{
    {"Rock", \{
            {"Paper",    "Paper"},
            {"Scissors", "Rock"},
            {"Rock",     "Draw"}}},
    {"Paper", \{
            {"Paper",    "Draw"},
            {"Scissors", "Scissors"},
            {"Rock",     "Paper"}}},
    {"Scissors", \{
            {"Paper",    "Scissors"},
            {"Scissors", "Draw"},
            {"Rock",     "Rock"}}},
  } in
  begin
    for each section in rules do
      if section.a == first then
        for each rule in section.b do
          if rule.a == second then return rule.b;
    return nil;
  end;
```

## Json Conversion 

Imagine for a brief moment that Json uses `int`s instead
of floats.

```
type Json is bool | int | nil | *i8 | *i8 -> Json | *Json

type Person is {
    .name *i8,
    .age int,
}

proc JsonToPerson[j:Json] ?*Person do
    switch type j as v
    case *Json then
        for each item in v do
            switch type item as person
            case *i8 -> Json then
                {:Person
                    GetName[person]^*i8,
                    GetAge[person]^int,
                }
            default return nil

proc GetName[person:*i8 -> Json] ?*i8 do
    switch type person["name"]^Json as name
    case *i8 then name

proc GetAge[person:*i8 -> Json] ?int do
    switch type person["age"]^Json as age
    case int then age
```