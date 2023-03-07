package pipelines

import (
	"abc/lexer"
	"abc/parser"
	"abc/resolution"
	"abc/typechecker"

	. "abc/core"
	lx "abc/core/lexeme"
	mod "abc/core/module"
	"io/ioutil"
)

// processes a single file and returns all tokens
// or an error
func Lexemes(file string) ([]*lx.Lexeme, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	st := lexer.NewLexer(file, s)
	return st.ReadAll()
}

// processes a single file and returns it's AST
// or an error
func Ast(file string) (*mod.Node, *Error) {
	s, err := getFile(file)
	if err != nil {
		return nil, err
	}
	return parser.Parse(file, s)
}

// processes a file and all it's dependencies
// returns a typed Module or an error
func Mod(file string) (*mod.Module, *Error) {
	return resolution.Resolve(file)
}

func TypedMod(file string) (*mod.Module, *Error) {
	m, err := Mod(file)
	if err != nil {
		return nil, err
	}
	err = typechecker.Check(m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

func Compile(file string) (string, *Error) {
	panic("unimplemented")
}

func getFile(file string) (string, *Error) {
	text, e := ioutil.ReadFile(file)
	if e != nil {
		return "", ProcessFileError(e)
	}
	return string(text), nil
}
