# anu programming language

Anu is a work-in-progress programming language.

```
proc main do print["Hello, World!\n"];
```

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
        3. [ProcLit](#proclit)
        4. [ProductLit](#productlit)
        5. [ArrayLit](#arraylit)
        6. [For](#for)
            1. [Conditional](#conditional)
            2. [Iterative](#iterative)
            3. [Range](#range)
        7. [Switch](#switch)
            1. [ValueSwitch](#valueswitch)
            2. [TypeSwitch](#typeswitch)
        8. [If](#if)
        9. [EarlyReturn](#earlyreturn)
        10. [Let](#let)
        11. [Set](#set)
4. [Type System](#typesystem)
    1. [Type Canonicalization](#typecanonicalization)
    2. [Type Identity](#typeidentity)
    3. [Type Equivalence](#typeequivalence)
    4. [Assignable](#assignable)
    5. [Castable](#castable)
    6. [Addressable](#addressable)
    7. [Uniqueness of References](#uniquenessofreferences)
5. [Misc]
    1. [Full Grammar](#fullgrammar)
    2. [Full Type Rules](#fullinferencerules)
    3. [Future](#future)
6. [Examples]
    1. [Rock, Paper, Scissors](#rockpaperscissors)

# Introduction <a name="introduction"/>

Anu is a procedural, statically typed programming language.
The most important characteristics are it's compile-time
memory management through use of unique pointers,
the lack of statements and the focus on algebraic data types.

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

The following is a list of all 41 operators and ponctuation tokens
in the language. All of them, including `-=`, `+=`, etc, are treated
as a single token.

```
    ;    =    [    ]     (    )    {    }
    ,    :    |    ->    .    *    ?    &
    ::   ==   !=   >     <    <=   >=   +
    -    ..   *    /     %    @    ~    <->
    ^    \    -=   +=    *=   /=   ..=  %=
    $
```

# Grammatical Elements <a name="grammar"/>

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

### Type <a name="type"/>

```
Type = "type" id (TypeAlias|TypeCreation|Enum)
TypeAlias = "as" TypeExpr
TypeCreation = "is" TypeExpr
```

The type declaration can be used to create
aliases to types and new types. An alias is _identical_
to the underlying type, while an new type is only _equivalent_
to it's underlying type. 

Given the following types:

```
type A is int
type B as int
```

The following propositions are true:

 - `A` is _equivalent_ to `int` and `B`
 - `A` is not _identical_ to `int` or `B`
 - `B` is _identical_ to `int`

Aliases cannot be recursive, while new types can be recursive as long
as there's a level of indirection between itself. Given the following types:

```
type A as {.value int .next &A}
type B is {.value int .next &B}
type C is {.value int .next C}
type D is string -> D
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

 - Two sum types are _equivalent_ if they contain _identical_ types, regardless of order
 - _Identical_ options inside a sum type are treated as a single option
 - Inside a sum type `void` is not considered

```
T | T == T
T | nil == nil | T
(int | string) | nil == nil | (string | int)
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
type Json   is nil | int | string | string->Json | *Json
```

#### Products <a name="products"/>

```
ProductType = "{" Naming+ "}"
Naming = ("." id)? UnaryType
```

Products are made by justaposition inside brackets `{}`,
the name of fields can be specified by the `.` operator,
otherwise the default is latin letters in alphabetical order:
`product.a`, `product.b`, ...,`product.z`, `product.aa`, 

 - Two product types are _equivalent_ if they have _identical_ types layed out in the same order.
 - A product type with a single field is _identical_ to that field's type.
 - Inside a product type `nil` is not considered.

```
{int int} == {.a int .b int} == {.x int .y int}
{int} == int
{nil int} == int
{nil nil int} == int

{T U} != {U T}
```

Product types are canonicalized like so:

```
{T}         => T
{nil T}     => T
{nil nil T} => T
{nil U T}   => {U T}
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

 - Two reference types are _equivalent_ if they have _identical_ base types.

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

 - Two procedural types are _equivalent_ if they have the same number
 of _identical_ parameters layed out in the same order,
 and the return types are _identical_

```
proc[T] T == proc[T] T
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

 - Two array types are _equivalent_ if they have _identical_ base types

```
*T == *T
*T != *U
```

Arrays have two special fields `length` and `cap`, both of type `int`,
that returns the length and total capacity of the array.

#### Map Type <a name="maptype"/>

```
Map = TypeFactor ("->" TypeFactor)*
```

Maps are built by using the association operator `->`.

 - Two map types are _equivalent_ if the keys are of _identical_ types,
and the values are of _identical_ types

```
string -> int == string -> int
string -> int != int -> string
```

The key of a map cannot be a reference or procedure type, it has to be
comparable.

#### Optional <a name="optionals"/>

```
UnaryType = TypePrefix* SingleType
TypePrefix = Ref | ArrayType | Optional
Optional = "?"
```

Option types are made by using the maybe operator `?`.

Options are canonicalized into sums:

```
?string  => nil | string
??string => nil | (nil | string)
?&string => nil | &string
&?string => &(nil | string)
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
type Something enum One is {int int},
                    Two
```

Can be desugared to:

```
type One is {int int}
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
 - `=` and `!=` are equality and inequality, they work on _comparable_ types and
the output is a `bool`.
 - `>`, `>=`, `<` and `<=` are greater, greater or equals, less, less or equals
respectivelly. They work on _orderable_ types and the output is a `bool`.
 - `is` is sum identity operator, it takes a sum and a type (that must be an option of the sum)
and outputs a `bool`.
 - `+` and `-` are the binary sum and subtraction operators, they work on integers
and the output is of the same type of it's operands.
 - `..` array concat operator, it takes two arrays of identical types and output
a new array with type identical to the operands. Since it copies the contents of the arrays,
it's subject to move semantics.
 - `*`, `/` and `%` are the multiplication, division and remainder operators. They work on integers
and the output is of the same type of it's operands.

### Prefix <a name="prefix"/>
```
Prefix = "&" | "@" | "not" | "~" | "$"
```

 - `&` is the address-of operator, it takes any _addressable_ expression and returns a reference to that value.
The output type is a reference of the expression type. It's subject to move semantics.
 - `@` is the value-at or dereferencing operator, it takes a reference and returns the base type.
It's subject to move semantics.
 - `not` is the logical operator not, it takes a bool and returns a bool.
 - `~` is the unary minus operator, it takes a single integer and outputs an integer.
 - `$` is the stringify operator, it takes a value of any type and prints a
string representation of it's values.

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

The parameters of a procedure must be of *identical* type to the formal
parameters defined in the procedure declaration, if named parameters
are not used, the order is important, and each parameter must match
the parameters in the declaration in order. If named parameters are used,
each parameter must be of *identical* type to the respectively named
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
is an option of the map's value type. (if a map has type `string -> int`
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
that is different from the type specified and returns a value of that
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
            default v
    let contents = switch type IO::ReadAll[a]^*i8 as v
                   case IO:Error then return v
                   default v
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
| ArrayLit
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

### NestedExpr <a name="nestedexpr"/>
```
NestedExpr = "(" Expr ")"
```
### Block <a name="block"/>
```
Block = "begin" ExprSemicolon* "end"
ExprSemicolon = Expr ";"?
```
### ProductLit <a name="productlit"/>
```
ProductLit = ComplexLitBody
ComplexLitBody = "{" (":" TypeExpr)? FieldList? "}"
FieldList = Field ("," Field)* ","?
Field = Expr ("=" Expr)?
```
### ArrayLit <a name="arraylit"/>
```
ArrayLit = "\" ComplexLitBody
```
### For <a name="for"/>
```
For = "for" (Conditional | Iterative | Range)
```
#### Conditional <a name="conditional"/>
```
Conditional = Expr "do" Expr
```
#### Iterative <a name="iterative"/>
```
Iterative = "each" id ("," id)? "in" Expr "do" Expr
```
#### Range <a name="range"/>
```
Range = "range" Expr "to" Expr ("as" id)? "do" Expr
```
### Switch <a name="switch"/>
```
Switch = "switch" (TypeSwitch | ValueSwitch)
```
#### ValueSwitch <a name="valueswitch"/>
```
ValueSwitch = Expr ValueCase* Default?
ValueCase = "case" ExprList "then" Expr
Default = "default" Expr
```
#### TypeSwitch <a name="typeswitch"/>
```
TypeSwitch = "type" Expr ("as" id) TypeCase* Default?
TypeCase = "case" TypeExprList "then" Expr
```
### If <a name="if"/>
```
If = "if" Expr "then" Expr ElseIf* Else?
ElseIf = "elseif" Expr "then" Expr
Else = "else" Expr
```
### EarlyReturn <a name="earlyreturn"/>
```
EarlyReturn = "return" Expr
```
### Let <a name="let"/>
```
Let = "let" LetDeclList ("in" Expr)?
LetDeclList = LetDecl ("," LetDecl)* ","?
LetDecl = VarList "=" Expr
VarList = Annotated ("," Annotated)* ","?
Annotated = id TypeAnnot?
```
### Set <a name="set"/>
```
Set = "set" ExprList assignOp Expr
assignOp = "=" | "-=" | "+=" | "*=" | "/=" | "..=" | "%=" | "<->" | "remove"
```
### New <a name="new"/>
```
New = "new" TypeAnnot? "[" Expr "," Expr ","? "]"
```

# Type System <a name="typesystem"/>
## Type Canonicalization <a name="typecanonicalization"/>

 - _Identical_ options inside a sum type are canonicalized into a single option
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

- New types are unique, two differently named types are never _identical_.
- New types declared in different modules are never _identical_.
- Aliases and anonymous types are _identical_ if they're _equivalent_.

## Type Equivalence <a name="typeequivalence"/>

Before two types can be compared for equivalency, they must first
be canonicalized.

Types are equivalent if they're structurally identical.

 - Two product types are _equivalent_ if they have _identical_ types
layed out in the same order.
 - Two sum types are _equivalent_ if they contain _identical_ types,
regardless of order
 - Two procedural types are _equivalent_ if they have the same number
 of _identical_ parameters layed out in the same order,
 and the return types are _identical_
 - Two map types are _equivalent_ if the keys are of _identical_ types,
and the values are of _identical_ types
 - Two array types are _equivalent_ if they have _identical_ base types
 - Two reference types are _equivalent_ if they have _identical_ base types.

## Type Assignability <a name="typeassignability"/>
## Type Castability <a name="typecastability"/>
## Uniqueness of References <a name="uniquenessofreferences"/>

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
TypeFactor = ProductType | UnaryType
ProductType = "{" Naming+ "}"
Naming = ("." id)? UnaryType
UnaryType = TypePrefix* SingleType
SingleType = Name | NestedType | ProcType | nil
TypePrefix = Ref | ArrayType | Optional
ArrayType = "*"
Optional = "?"
Ref = "&"
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
| ArrayLit
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

ArrayLit = "\" ComplexLitBody
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

New = "new" TypeAnnot? "[" Expr "," Expr ","? "]"

keyword = "import" | "from"    | "export" | "proc" | "const"  | "type"   |
          "begin"  | "end"     | "is"     | "or"   | "and"    | "not"    |
          "do"     | "for"     | "if"     | "each" | "in"     | "switch" |
          "case"   | "default" | "elseif" | "else" | "return" | "let"    |
          "set"    | "as"      | "to"     | "then" | "range"  | "nil"    |
          "new"    | "remove"  | "void"   | "true" | "false"

assignOp = "=" | "-=" | "+=" | "*=" | "/=" | "..=" | "%=" | "<->" | "remove"

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

# Future <a name="future"/>

 - FFI
 - Syntax sugar for chaining (without creating closures): `a \> f() \> g()` or `a \ f() \ g()`
 - Some form of reflection that returns stack trace information
 - Floats
 - Rank 1 polymorphism and inference

# Examples <a name="examples"/>
## Rock, Paper, Scissors <a name="rockpaperscissors"/>

```
proc Winner[first:string, second:string] ?string do
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
