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
            7. [Enum](#enum)
            8. [Optional](#optional)
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
    4. [Type Assignability](#typeassignability)
    5. [Type Castability](#typecastability)
    6. [Uniqueness of References](#uniquenessofreferences)
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
new     has      remove  true  false   void   
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
    -    ..   *    /     %    @    ~    .
    ^    \    -=   +=    *=   /=   ..=  %=
    <->
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
0_g3tg1d.anu
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

## Header <a name="header"/>

```
Header = (ModuleRelations ";"?)*
ModuleRelations = Import | Export | FromImport
```

A module header contains a series of module relations,
these define what is imported and exported from this module.
Module relations can only appear in the top of a file and
nowhere else.

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

const myWrite = IO::Writer
const myRead  = IO::Read
```

### FromImport <a name="fromimport"/>

```
FromImport = "from" id "import" AliasList
```

### Export <a name="export"/>

```
Export = "export" ("all" | AliasList)
```

## Body <a name="body"/>

```
Body = (Symbol ";"?)*
Symbol = Constant | Procedure | Type
```

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

### Constant <a name="constant"/>

```
Constant = "const" id TypeAnnot? "=" Expr
TypeAnnot = ":" TypeExpr
```

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
type Json   is nil | float | string | string->Json | *Json
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

Desugared to:

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
compOp = "==" | "!=" | ">" | ">=" | "<" | "<=" | "is" | "has"
Sum = Mult (sumOp Mult)*
sumOp = "+" | "-" | ".."
Mult = UnaryPrefix (multOp UnaryPrefix)*
multOp = "*" | "/" | "%"
UnaryPrefix = Prefix* UnarySuffix
UnarySuffix = Factor Suffix*
Prefix = "&" | "@" | "not" | "~"
Suffix
= PropertyAccess
| CallOrIndex
| TypeAnnot
| TypeReturn
```

Associativity is always left-to-right,
meaning `a + b + c` will be the same as `(a + b) + c`.
Precedence is defined directly in the grammar,
but can be viewed separatedly in the following table:

| Precedence | Operators                                    | Description          |
|:----------:|:--------------------------------------------:|:--------------------:|
|     0      | `or`                                         | Logical OR           |
|     1      | `and`                                        | Logical AND          |
|     2      | `=`, `!=`, `>`, `>=`, `<`, `<=`, `is`, `has` | Comparison and Order |
|     3      | `+`, `-`, `..`                               | Sum and Special      |
|     4      | `*`, `/`, `%`                                | Multiplicative       |
|     5      | `&`, `@`, `not`, `~`                         | Prefix (Unary)       |
|     6      | `.`, `[]`, `:`, `^`                          | Suffix               |

### Binary Operators <a name="binaryoperators"/>
### Prefix <a name="prefix"/>
### Suffix <a name="suffix"/>

#### Call/Index <a name="call"/>

```
CallOrIndex = "[" FieldList "]"
```

#### Property Access <a name="propertyaccess"/>

```
PropertyAccess = "." id
```

#### Bubble Up <a name="bubbleup"/>

```
TypeReturn = "^" TypeExpr
```

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
compOp = "==" | "!=" | ">"~("="|"-") | ">=" | "<"~("="|"-") | "<=" | "is" | "has"
Sum = Mult (sumOp Mult)*
sumOp = "+" | "-" | ".."
Mult = UnaryPrefix (multOp UnaryPrefix)*
multOp = "*" | "/" | "%"
UnaryPrefix = Prefix* UnarySuffix
UnarySuffix = Factor Suffix*
Prefix = "&" | "@" | "not" | "~"
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
          "new"    | "has"     | "remove" | "void" | "true"   | "false"

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

 - Syntax sugar for chaining (without creating closures): `a \> f() \> g()` or `a \ f() \ g()`
 - Some form of reflection that returns stack trace information

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
