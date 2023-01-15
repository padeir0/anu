package module

import (
	. "abc/core"
	lk "abc/core/module/lexkind"
	nk "abc/core/module/nodekind"
	T "abc/core/types"
)

type Lexeme struct {
	Text  string
	Kind  lk.LexKind
	Range Range
}

type Node struct {
	Lexeme *Lexeme
	Leaves []*Node

	Range Range
	Kind  nk.NodeKind
	Type  T.TypeID
}

type Symbol struct {
	Name string

	// Go doesn't have Sums :)
	Const    *ConstInfo
	Proc     *ProcInfo
	Type     *TypeInfo
	Let      *LetInfo
	Alias    *AliasInfo
	Argument *ArgumentInfo
	External *ExternalInfo

	Root       *Node
	References []*Node
}

type ConstInfo struct {
	Type *T.TypeStructure
}
type ProcInfo struct {
	Type *T.TypeStructure
}
type TypeInfo struct {
	Type   *T.TypeStructure
	Fields map[string]int
}
type LetInfo struct {
	Type      *T.TypeStructure
	IsMutated bool
}
type ArgumentInfo struct {
	Type *T.TypeStructure
}
type AliasInfo struct {
	Type *T.TypeStructure
}
type ExternalInfo struct {
	// because of aliases
	RealName     string
	SourceModule string
}

/*
	Universe -> depth == 0
	Global   -> depth == 1
	Local    -> depth >= 2
*/
type Scope struct {
	Parent  *Scope
	Symbols map[string]*Symbol
	Depth   int
}

type Dependency struct {
	M      *Module
	Source *Node
}

type Module struct {
	Name     string
	FullPath string
	BasePath string

	Root *Node

	Global *Scope

	Dependencies map[string]*Dependency
	// possibly aliased
	Exported map[string]*Symbol

	Visited bool
}
