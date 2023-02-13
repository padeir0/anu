package module

import (
	. "abc/core"
	lex "abc/core/lexeme"
	nk "abc/core/module/nodekind"
	T "abc/core/types"
	"fmt"
)

type Node struct {
	Lexeme *lex.Lexeme
	Leaves []*Node

	Kind  nk.NodeKind
	T     *T.Type
	Range *Range
}

func (n *Node) String() string {
	return ast(n, 0)
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
