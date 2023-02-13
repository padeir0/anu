package lexer

import (
	lx "abc/core/lexeme"
	T "abc/core/lexeme/lexkind"

	. "abc/core"
	et "abc/core/errorkind"
	sv "abc/core/severity"

	"fmt"
	"strings"
	"unicode/utf8"
)

const IsTracking bool = false

func Track(st *Lexer, s string) {
	if IsTracking {
		fmt.Printf("%v: %v\n", s, st.Word.String())
	}
}

func NewLexerError(st *Lexer, t et.ErrorKind, message string) *Error {
	loc := st.GetSourceLocation()
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: loc,
		Message:  message,
	}
}

func AllTokens(s *Lexer) []*lx.Lexeme {
	output := []*lx.Lexeme{}
	err := s.Next()
	if err != nil {
		panic(err)
	}
	for s.Word.Kind != T.EOF {
		output = append(output, s.Word)
		err = s.Next()
		if err != nil {
			panic(err)
		}
	}
	return output
}

type Lexer struct {
	Word *lx.Lexeme

	File                string
	BeginLine, BeginCol int
	EndLine, EndCol     int

	Start, End   int
	LastRuneSize int
	Input        string

	Peeked *lx.Lexeme
}

func NewLexer(filename string, s string) *Lexer {
	st := &Lexer{
		File:  filename,
		Input: s,
	}
	return st
}

func (this *Lexer) GetSourceLocation() *Location {
	rng := this.Range()
	return &Location{
		File:  this.File,
		Range: &rng,
	}
}

func (this *Lexer) Next() *Error {
	if this.Peeked != nil {
		p := this.Peeked
		this.Peeked = nil
		this.Word = p
		return nil
	}
	symbol, err := any(this)
	if err != nil {
		return err
	}
	this.Start = this.End // this shouldn't be here
	this.BeginLine = this.EndLine
	this.BeginCol = this.EndCol
	this.Word = symbol
	return nil
}

func (this *Lexer) Peek() (*lx.Lexeme, *Error) {
	symbol, err := any(this)
	if err != nil {
		return nil, err
	}
	this.Start = this.End
	this.Peeked = symbol
	return symbol, nil
}

func (this *Lexer) ReadAll() ([]*lx.Lexeme, *Error) {
	e := this.Next()
	if e != nil {
		return nil, e
	}
	output := []*lx.Lexeme{}
	for this.Word.Kind != T.EOF {
		output = append(output, this.Word)
		e = this.Next()
		if e != nil {
			return nil, e
		}
	}
	return output, nil
}

func (this *Lexer) Selected() string {
	return this.Input[this.Start:this.End]
}

func (this *Lexer) Range() Range {
	return Range{
		Begin: Position{
			Line:   this.BeginLine,
			Column: this.BeginCol,
		},
		End: Position{
			Line:   this.EndLine,
			Column: this.EndCol,
		},
	}
}

func genNumNode(l *Lexer, tp T.LexKind, value int64) *lx.Lexeme {
	text := l.Selected()
	n := &lx.Lexeme{
		Kind:  tp,
		Text:  text,
		Value: value,
		Range: l.Range(),
	}
	return n
}

func genNode(l *Lexer, tp T.LexKind) *lx.Lexeme {
	text := l.Selected()
	n := &lx.Lexeme{
		Kind:  tp,
		Text:  text,
		Range: l.Range(),
	}
	return n
}

func nextRune(l *Lexer) rune {
	r, size := utf8.DecodeRuneInString(l.Input[l.End:])
	if r == utf8.RuneError && size == 1 {
		panic("Invalid UTF8 rune in string")
	}
	l.End += size
	l.LastRuneSize = size

	if r == '\n' {
		l.EndLine++
		l.EndCol = 0
	} else {
		l.EndCol++
	}

	return r
}

func peekRune(l *Lexer) rune {
	r, size := utf8.DecodeRuneInString(l.Input[l.End:])
	if r == utf8.RuneError && size == 1 {
		panic("Invalid UTF8 rune in string")
	}

	return r
}

/*ignore ignores the text previously read*/
func ignore(l *Lexer) {
	l.Start = l.End
	l.BeginLine = l.EndLine
	l.BeginCol = l.EndCol
	l.LastRuneSize = 0
}

func acceptRun(l *Lexer, s string) {
	r := peekRune(l)
	for strings.ContainsRune(s, r) {
		nextRune(l)
		r = peekRune(l)
	}
}

func acceptUntil(l *Lexer, s string) {
	r := peekRune(l)
	for !strings.ContainsRune(s, r) {
		nextRune(l)
		r = peekRune(l)
	}
}

const (
	/*eof is equivalent to RuneError, but in this package it only shows up in EoFs
	If the rune is invalid, it panics instead*/
	eof rune = utf8.RuneError
)

const (
	insideStr  = `\"`
	insideChar = `\'`
	digits     = "0123456789"
	hex_digits = "0123456789ABCDEFabcdef"
	bin_digits = "01"
	letters    = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_" // yes _ is a letter, fuck you
)

func isNumber(r rune) bool {
	return strings.ContainsRune(digits, r)
}

func isLetter(r rune) bool {
	return strings.ContainsRune(letters, r)
}

func ignoreWhitespace(st *Lexer) {
	r := peekRune(st)
loop:
	for {
		switch r {
		case ' ', '\t', '\n':
			nextRune(st)
		case '#':
			comment(st)
		default:
			break loop
		}
		r = peekRune(st)
	}
	ignore(st)
}

// refactor this
func any(st *Lexer) (*lx.Lexeme, *Error) {
	var r rune
	var tp T.LexKind

	ignoreWhitespace(st)

	r = peekRune(st)

	if isNumber(r) {
		return number(st), nil
	}
	if isLetter(r) {
		return identifier(st), nil
	}
	if r == '\'' {
		return charLit(st), nil
	}
	if r == '"' {
		return strLit(st), nil
	}

	switch r {
	case '+':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.PlusAssign
		default:
			tp = T.Plus
		}
	case '-':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.MinusAssign
		case '>':
			nextRune(st)
			tp = T.Arrow
		default:
			tp = T.Minus
		}
	case '*':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.MultiplicationAssign
		default:
			tp = T.Star
		}
	case ':':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case ':':
			nextRune(st)
			tp = T.DoubleColon
		default:
			tp = T.Colon
		}
	case '>': // >  >=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.GreaterOrEquals
		default:
			tp = T.Greater
		}
	case '<': // <  <->  <=
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.LessOrEquals
		case '-':
			nextRune(st)
			r = peekRune(st)
			if r != '>' {
				message := fmt.Sprintf("Invalid symbol: %v", string(r))
				err := NewLexerError(st, et.InvalidSymbol, message)
				return nil, err
			}
			tp = T.Swap
		default:
			tp = T.Less
		}
	case '!':
		nextRune(st)
		r = peekRune(st)
		switch r {
		case '=':
			nextRune(st)
			tp = T.Different
		default:
			message := fmt.Sprintf("Invalid symbol: %v", string(r))
			err := NewLexerError(st, et.InvalidSymbol, message)
			return nil, err
		}
	case '=':
		nextRune(st)
		r = peekRune(st)
		if r == '=' {
			nextRune(st)
			tp = T.Equals
		} else {
			tp = T.Assign
		}
	case '&':
		nextRune(st)
		tp = T.Ampersand
	case '?':
		nextRune(st)
		tp = T.QuestionMark
	case '|':
		nextRune(st)
		tp = T.Pipe
	case '\\':
		nextRune(st)
		tp = T.Slash
	case '/':
		nextRune(st)
		tp = T.Division
	case '%':
		nextRune(st)
		tp = T.Remainder
	case '@':
		nextRune(st)
		tp = T.At
	case '~':
		nextRune(st)
		tp = T.Tilde
	case '$':
		nextRune(st)
		tp = T.Dollar
	case '^':
		nextRune(st)
		tp = T.Caret
	case '(':
		nextRune(st)
		tp = T.LeftParen
	case ')':
		nextRune(st)
		tp = T.RightParen
	case '{':
		nextRune(st)
		tp = T.LeftBrace
	case '}':
		nextRune(st)
		tp = T.RightBrace
	case '[':
		nextRune(st)
		tp = T.LeftBracket
	case ']':
		nextRune(st)
		tp = T.RightBracket
	case ',':
		nextRune(st)
		tp = T.Comma
	case ';':
		nextRune(st)
		tp = T.Semicolon
	case '.':
		nextRune(st)
		r := peekRune(st)
		if r == '.' {
			nextRune(st)
			r := peekRune(st)
			if r == '=' {
				nextRune(st)
				tp = T.ConcatAssign
			} else {
				tp = T.DoubleDot
			}
		} else {
			tp = T.Dot
		}
	case eof:
		nextRune(st)
		return &lx.Lexeme{Kind: T.EOF}, nil
	default:
		message := fmt.Sprintf("Invalid symbol: %v", string(r))
		err := NewLexerError(st, et.InvalidSymbol, message)
		return nil, err
	}
	return genNode(st, tp), nil
}

// sorry
func number(st *Lexer) *lx.Lexeme {
	r := peekRune(st)
	var value int64
	if r == '0' {
		nextRune(st)
		r = peekRune(st)
		switch r {
		case 'x': // he x
			nextRune(st)
			acceptRun(st, hex_digits)
			value = parseHex(st.Selected())
		case 'b': // b inary
			nextRune(st)
			acceptRun(st, bin_digits)
			value = parseBin(st.Selected())
		default:
			acceptRun(st, digits)
			value = parseNormal(st.Selected())
		}
	} else {
		acceptRun(st, digits)
		value = parseNormal(st.Selected())
	}
	r = peekRune(st)
	switch r {
	case 'l':
		nextRune(st)
		r = peekRune(st)
		if r == 'l' { // longer long
			nextRune(st)
			return genNumNode(st, T.I32Lit, value)
		} else { // long
			return genNumNode(st, T.I64Lit, value)
		}
	case 's':
		nextRune(st)
		r = peekRune(st)
		if r == 's' { // shorter short
			nextRune(st)
			return genNumNode(st, T.I8Lit, value)
		} else { // short
			return genNumNode(st, T.I16Lit, value)
		}
	}
	return genNumNode(st, T.IntLit, value)
}

func identifier(st *Lexer) *lx.Lexeme {
	r := peekRune(st)
	if !isLetter(r) {
		panic("identifier not beginning with letter")
	}
	acceptRun(st, digits+letters)
	selected := st.Selected()
	tp := T.Ident
	switch selected {
	case "import":
		tp = T.Import
	case "from":
		tp = T.From
	case "export":
		tp = T.Export
	case "proc":
		tp = T.Proc
	case "const":
		tp = T.Const
	case "type":
		tp = T.Type
	case "begin":
		tp = T.Begin
	case "end":
		tp = T.End
	case "is":
		tp = T.Is
	case "or":
		tp = T.Or
	case "and":
		tp = T.And
	case "not":
		tp = T.Not
	case "do":
		tp = T.Do
	case "for":
		tp = T.For
	case "if":
		tp = T.If
	case "each":
		tp = T.Each
	case "in":
		tp = T.In
	case "switch":
		tp = T.Switch
	case "case":
		tp = T.Case
	case "default":
		tp = T.Default
	case "elseif":
		tp = T.Elseif
	case "else":
		tp = T.Else
	case "return":
		tp = T.Return
	case "let":
		tp = T.Let
	case "set":
		tp = T.Set
	case "as":
		tp = T.As
	case "to":
		tp = T.To
	case "then":
		tp = T.Then
	case "range":
		tp = T.Range
	case "nil":
		tp = T.Nil
	case "new":
		tp = T.New
	case "remove":
		tp = T.Remove
	case "true":
		tp = T.True
	case "false":
		tp = T.False
	case "void":
		tp = T.Void
	}
	return genNode(st, tp)
}

func comment(st *Lexer) *Error {
	r := nextRune(st)
	if r != '#' {
		panic("internal error: comment without '#'")
	}
	for !strings.ContainsRune("\n"+string(eof), r) {
		nextRune(st)
		r = peekRune(st)
	}
	nextRune(st)
	return nil
}

func strLit(st *Lexer) *lx.Lexeme {
	r := nextRune(st)
	if r != '"' {
		panic("wong")
	}
	for {
		acceptUntil(st, insideStr)
		r := peekRune(st)
		if r == '"' {
			nextRune(st)
			return &lx.Lexeme{
				Text:  st.Selected(),
				Kind:  T.StringLit,
				Range: st.Range(),
			}
		}
		if r == '\\' {
			nextRune(st) // \
			nextRune(st) // escaped rune
		}
	}
}

func charLit(st *Lexer) *lx.Lexeme {
	r := nextRune(st)
	if r != '\'' {
		panic("wong")
	}
	for {
		acceptUntil(st, insideChar)
		r := peekRune(st)
		if r == '\'' {
			nextRune(st)
			text := st.Selected()
			return &lx.Lexeme{
				Text:  text,
				Kind:  T.CharLit,
				Range: st.Range(),
				Value: parseCharLit(text[1 : len(text)-1]),
			}
		}
		if r == '\\' {
			nextRune(st) // \
			nextRune(st) // escaped rune
		}
	}
}

func IsValidIdentifier(s string) bool {
	st := NewLexer("oh no", s)
	tks, err := st.ReadAll()
	if err != nil {
		return false
	}
	if len(tks) != 1 { // we want only ID
		return false
	}
	return tks[0].Kind == T.Ident
}

func parseNormal(text string) int64 {
	var output int64 = 0
	for i := range text {
		output *= 10
		char := text[i]
		if char >= '0' || char <= '9' {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func parseHex(oldText string) int64 {
	text := oldText[2:]
	var output int64 = 0
	for i := range text {
		output *= 16
		char := text[i]
		if char >= '0' && char <= '9' {
			output += int64(char - '0')
		} else if char >= 'a' && char <= 'f' {
			output += int64(char-'a') + 10
		} else if char >= 'A' && char <= 'F' {
			output += int64(char-'A') + 10
		} else {
			panic(text)
		}
	}
	return output
}

func parseBin(oldText string) int64 {
	text := oldText[2:]
	var output int64 = 0
	for i := range text {
		output *= 2
		char := text[i]
		if char == '0' || char == '1' {
			output += int64(char - '0')
		} else {
			panic(text)
		}
	}
	return output
}

func parseCharLit(text string) int64 {
	value := int64(text[0])
	if len(text) > 1 {
		switch text {
		case "\\n":
			value = '\n'
		case "\\t":
			value = '\t'
		case "\\r":
			value = '\r'
		case "\\'":
			value = '\''
		case "\\\"":
			value = '"'
		case "\\\\":
			value = '\\'
		default:
			fmt.Println(text)
			panic("too many chars in char :C")
		}
	}
	return value
}
