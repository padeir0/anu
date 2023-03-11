package lexkind

type LexKind int

func (this LexKind) String() string {
	return mapToString[this]
}

const (
	InvalidLexKind LexKind = iota

	Ident
	IntLit
	I64Lit
	I32Lit
	I16Lit
	I8Lit
	StringLit
	CharLit

	Equals
	Different
	Greater
	GreaterOrEquals
	Less
	LessOrEquals
	Plus
	Minus
	DoubleDot
	Star
	Division
	Remainder
	Ampersand
	At
	Dollar
	Tilde
	Dot
	Comma
	Caret
	QuestionMark
	Arrow
	Pipe
	Colon
	DoubleColon
	Semicolon
	LeftParen
	RightParen
	LeftBracket
	RightBracket
	LeftBrace
	RightBrace
	Slash

	Assign
	PlusAssign
	MinusAssign
	MultiplicationAssign
	ConcatAssign
	Swap

	Import
	From
	Export
	Proc
	Const
	Type
	Begin
	End
	Is
	Or
	And
	Not
	Do
	For
	If
	While
	In
	Switch
	Match
	Case
	Default
	Elseif
	Else
	Return
	Let
	Set
	As
	To
	Then
	Range
	Nil
	New
	Remove
	True
	False
	Void
	All
	Enum

	EOF
)

var mapToString = map[LexKind]string{
	Ident: "id",

	IntLit:    "int lit",
	I64Lit:    "i64 lit",
	I32Lit:    "i32 lit",
	I16Lit:    "i16 lit",
	I8Lit:     "i8 lit",
	StringLit: "string lit",
	CharLit:   "char lit",

	Equals:          "==",
	Different:       "!=",
	Greater:         ">",
	GreaterOrEquals: ">=",
	Less:            "<",
	LessOrEquals:    "<=",
	Plus:            "+",
	Minus:           "-",
	DoubleDot:       "..",
	Star:            "*",
	Division:        "/",
	Remainder:       "%",
	Ampersand:       "&",
	At:              "@",
	Dollar:          "$",
	Tilde:           "~",
	Dot:             ".",
	Comma:           ",",
	Caret:           "^",
	QuestionMark:    "?",
	Arrow:           "->",
	Pipe:            "|",
	Colon:           ":",
	DoubleColon:     "::",
	Semicolon:       ";",
	LeftParen:       "(",
	RightParen:      ")",
	LeftBracket:     "[",
	RightBracket:    "]",
	LeftBrace:       "{",
	RightBrace:      "}",
	Slash:           "\\",

	Assign:               "=",
	PlusAssign:           "+=",
	MinusAssign:          "-=",
	MultiplicationAssign: "*=",
	ConcatAssign:         "..=",
	Swap:                 "<->",

	Import:  "import",
	From:    "from",
	Export:  "export",
	Proc:    "proc",
	Const:   "const",
	Type:    "type",
	Begin:   "begin",
	End:     "end",
	Is:      "is",
	Or:      "or",
	And:     "and",
	Not:     "not",
	Do:      "do",
	For:     "for",
	If:      "if",
	While:   "while",
	In:      "in",
	Switch:  "switch",
	Match:   "match",
	Case:    "case",
	Default: "default",
	Elseif:  "elseif",
	Else:    "else",
	Return:  "return",
	Let:     "let",
	Set:     "set",
	As:      "as",
	To:      "to",
	Then:    "then",
	Range:   "range",
	Nil:     "nil",
	New:     "new",
	Remove:  "remove",
	True:    "true",
	False:   "false",
	Void:    "void",

	EOF: "EOF",
}
