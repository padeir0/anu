package errorkind

import "strconv"

type ErrorKind int

func (et ErrorKind) String() string {
	v, ok := ErrorCodeMap[et]
	if !ok {
		panic(strconv.FormatInt(int64(et), 10) + "is not stringified")
	}
	return v
}

const (
	InvalidErrType ErrorKind = iota
	InternalCompilerError
	FileError
	InvalidSymbol
	ExpectedSymbol
	ExpectedEOF
	ExpectedProd
	InvalidFileName
	NameAlreadyDefined
	DuplicatedExport
	ExportingUndefName
	NameNotExported
	InvalidDependencyCycle
	AmbiguousModuleName
	ModuleNotFound
	InvalidConstCycle
	InvalidTypeCycle
	SymbolNotDeclared
	ExpectedType
	InvalidField
	InvalidConstExpr
)

var ErrorCodeMap = map[ErrorKind]string{
	InvalidErrType:         "E001",
	InternalCompilerError:  "E002",
	FileError:              "E003",
	InvalidSymbol:          "E004",
	ExpectedSymbol:         "E005",
	ExpectedEOF:            "E006",
	ExpectedProd:           "E007",
	InvalidFileName:        "E008",
	NameAlreadyDefined:     "E009",
	DuplicatedExport:       "E010",
	ExportingUndefName:     "E011",
	NameNotExported:        "E012",
	InvalidDependencyCycle: "E013",
	AmbiguousModuleName:    "E014",
	ModuleNotFound:         "E015",
	InvalidConstCycle:      "E016",
	InvalidTypeCycle:       "E017",
	SymbolNotDeclared:      "E018",
	ExpectedType:           "E019",
	InvalidField:           "E020",
	InvalidConstExpr:       "E021",
}
