package nodekind

type NodeKind int

const (
	InvalidNodeType NodeKind = iota
	Terminal

	ExprList
	TypeList

//  ...
)
