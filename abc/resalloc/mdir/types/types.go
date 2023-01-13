package types

type Type int

const (
	InvalidType Type = iota

	Bool
	I8
	I16
	I32
	I64
	Ptr
	Proc
)

// returns the size of this type in bytes
// panics if the type is special
func (this Type) Size() int {
	switch this {
	case Bool:
		return 1
	case I8:
		return 1
	case I16:
		return 2
	case I32:
		return 4
	case I64:
		return 8
	case Ptr:
		return 8
	case Proc:
		return 8
	}
	panic("unsizeable type")
}

func IsValid(t Type) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr ||
		t == Bool ||
		t == Proc
}

func IsProc(t Type) bool {
	return t == Proc
}

func IsNumber(t Type) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr
}

func IsPtr(t Type) bool {
	return t == Ptr
}

func IsBool(t Type) bool {
	return t == Bool
}

func Castable(t Type) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr ||
		t == Bool
}
