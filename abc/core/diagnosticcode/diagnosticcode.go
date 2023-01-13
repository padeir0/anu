package diagnosticcode

type DiagnosticCode int

const (
	InvalidDiagnosticCode DiagnosticCode = iota
	InternalCompilerError
	InvalidCharacter
)

var toStr = map[DiagnosticCode]string{
	InternalCompilerError: "XXXX",
	InvalidCharacter:      "E001",
}
