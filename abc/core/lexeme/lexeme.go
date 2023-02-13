package lexeme

import (
	. "abc/core"
	lk "abc/core/lexeme/lexkind"
)

type Lexeme struct {
	Text  string
	Kind  lk.LexKind
	Value int64
	Range Range
}

func (this Lexeme) String() string {
	return "(" + this.Text + ", " + this.Kind.String() + ")"
}
