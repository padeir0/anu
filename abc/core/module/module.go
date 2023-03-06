package module

import (
	. "abc/core"
	lex "abc/core/lexeme"
	T "abc/core/types"

	ek "abc/core/errorkind"
	nk "abc/core/module/nodekind"
	sk "abc/core/module/symbolkind"
	sv "abc/core/severity"

	"fmt"
	"strings"
)

type Node struct {
	Lexeme *lex.Lexeme
	Leaves []*Node

	Kind  nk.NodeKind
	T     *T.Type
	Range *Range

	// only for scoped nodes like Block, For, etc
	Scope *Scope
}

func (this *Node) String() string {
	return ast(this, 0)
}

func (this *Node) AddLeaf(other *Node) {
	if this.Leaves == nil {
		this.Leaves = []*Node{other}
	} else {
		this.Leaves = append(this.Leaves, other)
	}
}

func ast(n *Node, i int) string {
	if n == nil {
		return "nil"
	}
	rng := "nil"
	if n.Range != nil {
		rng = n.Range.String()
	}
	output := fmt.Sprintf("{%v, %v, %v, %v",
		n.Lexeme,
		n.Kind,
		n.T.String(),
		rng,
	)
	output += "}"
	for _, kid := range n.Leaves {
		if kid == nil {
			output += indent(i) + "nil"
			continue
		}
		output += indent(i) + ast(kid, i+1)
	}
	return output
}

func indent(n int) string {
	output := "\n"
	for i := -1; i < n-1; i++ {
		output += "    "
	}
	output += "└─>"
	return output
}

var Universe *Scope = &Scope{
	Parent: nil,
	Symbols: map[string]*Symbol{
		"i8":   {Name: "i8", Kind: sk.Builtin},
		"i16":  {Name: "i16", Kind: sk.Builtin},
		"i32":  {Name: "i32", Kind: sk.Builtin},
		"i64":  {Name: "i64", Kind: sk.Builtin},
		"int":  {Name: "int", Kind: sk.Builtin},
		"bool": {Name: "bool", Kind: sk.Builtin},
		"nil":  {Name: "nil", Kind: sk.Builtin},
		"void": {Name: "void", Kind: sk.Builtin},
	},
}

func Place(M *Module, n *Node) *Location {
	return &Location{
		File:  M.FullPath,
		Range: n.Range,
	}
}

func NewError(M *Module, t ek.ErrorKind, n *Node, message string) *Error {
	loc := Place(M, n)
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: loc,
		Message:  message,
	}
}

type Module struct {
	BasePath string
	Name     string
	FullPath string
	Root     *Node

	Global *Scope

	Dependencies map[string]*Dependency
	Exported     map[string]*Symbol

	Visited bool
}

func (this *Module) ResetVisited() {
	if !this.Visited {
		return
	}
	this.Visited = false
	for _, dep := range this.Dependencies {
		dep.M.ResetVisited()
	}
}

type Dependency struct {
	M      *Module
	Source *Node
}

type Scope struct {
	Parent  *Scope
	Symbols map[string]*Symbol
}

func (this *Scope) String() string {
	output := []string{}
	for _, sy := range this.Symbols {
		output = append(output, sy.String())
	}
	return "{" + strings.Join(output, ", ") + "}"
}

func (this *Scope) Find(name string) *Symbol {
	v, ok := this.Symbols[name]
	if ok {
		return v
	}
	if this.Parent == nil {
		return nil
	}
	return this.Parent.Find(name)
}

func (this *Scope) Add(name string, sy *Symbol) {
	_, ok := this.Symbols[name]
	if ok {
		panic("name already added")
	}
	this.Symbols[name] = sy
}

type Reference struct {
	Ignore bool
	S      *Symbol
}

type Symbol struct {
	Kind sk.SymbolKind
	Name string
	N    *Node
	M    *Module

	// only for globals
	Refs     map[string]*Reference
	External bool
}

func (this *Symbol) String() string {
	switch this.Kind {
	case sk.Procedure:
		return "proc " + this.Name
	case sk.Local:
		return "local " + this.Name
	case sk.Argument:
		return "arg " + this.Name
	case sk.Constant:
		return "const " + this.Name
	case sk.TypeAlias:
		return "type alias " + this.Name
	case sk.TypeEnum:
		return "enum " + this.Name
	case sk.TypeCreation:
		return "new type " + this.Name
	case sk.Module:
		return "module " + this.Name
	case sk.Builtin:
		return "builtin " + this.Name
	default:
		return "invalid"
	}
}

func (g *Symbol) Link(other *Symbol) {
	_, ok := g.Refs[other.Name]
	if ok {
		return
	}
	g.Refs[other.Name] = &Reference{S: other}
}

func (g *Symbol) IgnorableLink(other *Symbol) {
	_, ok := g.Refs[other.Name]
	if ok {
		return
	}
	g.Refs[other.Name] = &Reference{Ignore: true, S: other}
}
