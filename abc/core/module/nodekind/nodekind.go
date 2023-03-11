package nodekind

type NodeKind int

func (this NodeKind) String() string {
	switch this {
	case Terminal:
		return "term"
	case Block:
		return "block"
	case Module:
		return "module"
	case Header:
		return "header"
	case Body:
		return "body"
	case IdList:
		return "identifier list"
	case Sum:
		return "sum"
	case Product:
		return "product"
	case TypeArgs:
		return "type arguments"
	case Signature:
		return "signature"
	case Arguments:
		return "arguments"
	case CallOrIndex:
		return "call or index"
	case ProductLit:
		return "product literal"
	case ArrayMapLit:
		return "array literal"
	case FieldList:
		return "field list"
	case ExpressionList:
		return "expression list"
	case LetDeclList:
		return "let declaration list"
	case VarList:
		return "variable list"
	case ElseIfList:
		return "elseif list"
	case TypeCaseList:
		return "type case list"
	case TypeExprList:
		return "type expression list"
	case ValueCaseList:
		return "value case switch"
	}
	panic("you forgot")
}

const (
	InvalidNodeKind NodeKind = iota
	Terminal
	Module
	Header
	Body
	IdList
	Sum
	Product
	TypeArgs
	Signature
	Arguments
	CallOrIndex
	Block
	ProductLit
	ArrayMapLit
	FieldList
	ExpressionList
	LetDeclList
	VarList
	ElseIfList
	TypeExprList

	ValueCaseList
	TypeCaseList
)
