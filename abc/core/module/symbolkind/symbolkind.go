package symbolkind

type SymbolKind int

func (this SymbolKind) IsType() bool {
	switch this {
	case TypeAlias, TypeEnum, TypeEnumOption, TypeCreation:
		return true
	}
	return false
}

const (
	InvalidSymbolKind SymbolKind = iota
	// Global scope
	TypeAlias
	TypeEnum
	TypeEnumOption
	TypeCreation
	Constant
	Procedure
	Module
	// Local scope
	Argument
	Local
)
