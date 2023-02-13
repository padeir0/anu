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
)

var ErrorCodeMap = map[ErrorKind]string{
	InvalidErrType:        "E001",
	InternalCompilerError: "E002",
	FileError:             "E003",
	InvalidSymbol:         "E004",
	ExpectedSymbol:        "E005",
	ExpectedEOF:           "E006",
	ExpectedProd:          "E007",
}
