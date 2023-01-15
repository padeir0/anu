package planartypes

// planar representation of types
type Type struct {
	Rows [][]*SimpleType
}

// unions have more than one representation
// so types become 2D
//     type Example is {i8, {i8 i8} | {i64 i64}, i64}
// becomes:
//      ______       index
//     | i8  |         0
//     | tag |______   1
//     |  i8 | i64 |   2
//     |  i8 | i64 |   3
//     | i64 |         4
//
//
// lowering arrays
//    type ascii is *i8
// becomes:
//    ptr -> [length, capacity, items...]

// SimpleType = Basic | PtrTo | Proc | Region
type SimpleType struct {
	Basic *BasicType
	PtrTo *Type
	Proc  *ProcType
	// a continuous region, used to represent arrays,
	// a region must be the last unidimensional
	// property of any type
	Region *Type
}

// returns the size of this type in bytes
// panics if the type is special
func (this *SimpleType) Size() int {
	if this.PtrTo != nil {
		return 8
	}
	if this.Proc != nil {
		return 8
	}
	switch *this.Basic {
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
	}
	if this.Region != nil {
		panic("regions have dynamic size!")
	}
	panic("unsizeable type")
}

func (this *Type) IsValid() bool {
	if len(this.Rows) == 0 {
		return false
	}
	count, row, col := this.findRegions()
	// must be a single one
	if count > 1 {
		return false
	}
	// has to be the last non union field of a type
	if row != len(this.Rows)-1 || col != 0 {
		return false
	}
	return true
}

func (this *Type) findRegions() (int, int, int) {
	regionCount := 0
	lastRow := -1
	lastCol := -1
	for r, row := range this.Rows {
		for c, col := range row {
			if col.Region != nil {
				regionCount += 1
				lastRow = r
				lastCol = c
			}
		}
	}
	return regionCount, lastRow, lastCol
}

type ProcType struct {
	Arguments []BasicType
	Returns   []BasicType
	Locals    []BasicType
}

func (this *ProcType) StrRets() string {
	return StringTypes(this.Returns)
}

func (this *ProcType) StrArgs() string {
	return StringTypes(this.Arguments)
}

func (this *ProcType) StrLocals() string {
	return StringTypes(this.Locals)
}

func StringTypes(tps []BasicType) string {
	if len(tps) == 0 {
		return ""
	}
	if len(tps) == 1 {
		return tps[0].String()
	}
	output := tps[0].String()
	for _, t := range tps[1:] {
		output += ", " + t.String()
	}
	return output
}

type BasicType int

func (this BasicType) String() string {
	switch this {
	case Bool:
		return "bool"
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	// void pointer
	case Ptr:
		return "ptr"
	}
	panic("unsizeable type")
}

const (
	InvalidType BasicType = iota

	Bool
	I8
	I16
	I32
	I64
	// void pointer
	Ptr
)

func IsValid(t BasicType) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr ||
		t == Bool
}

func IsProc(t *SimpleType) bool {
	return t.Proc != nil
}

func IsNumber(t BasicType) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr
}

func IsPtr(t BasicType) bool {
	return t == Ptr
}

func IsBool(t BasicType) bool {
	return t == Bool
}

func IsCastable(t BasicType) bool {
	return t == I8 ||
		t == I16 ||
		t == I32 ||
		t == I64 ||
		t == Ptr ||
		t == Bool
}
