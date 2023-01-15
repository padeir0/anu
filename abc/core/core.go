// defines core types and functions used throught the compiler
package core

import (
	dc "abc/core/diagnosticcode"
	"strconv"
)

type Position struct {
	Line   int
	Column int
}

func (this Position) LessThan(other Position) bool {
	if this.Line == other.Line {
		return this.Column < other.Column
	}
	return this.Line < other.Line
}

func (this Position) MoreOrEqualsThan(other Position) bool {
	if this.Line == other.Line {
		return this.Column >= other.Column
	}
	return this.Line > other.Line
}

func (this Position) IsInside(rng Range) bool {
	return this.MoreOrEqualsThan(rng.Begin) && this.LessThan(rng.End)
}

func (this Position) String() string {
	return strconv.FormatInt(int64(this.Line), 10) + ":" +
		strconv.FormatInt(int64(this.Column), 10)
}

type Range struct {
	Begin Position
	End   Position // exclusive
}

func (this Range) String() string {
	return this.Begin.String() + " to " + this.End.String()
}

type Location struct {
	ModuleName string
	Range      Range
}

func (this Location) String() string {
	return this.ModuleName + ":" +
		this.Range.String()
}

type Severity int

func (this Severity) String() string {
	switch this {
	case Error:
		return "error"
	case Warning:
		return "warning"
	case Information:
		return "info"
	case Hint:
		return "hint"
	case InternalError:
		return "internal error"
	}
	panic("invalid severity")
}

const (
	InvalidSeverity Severity = iota
	Error
	Warning
	Information
	Hint
	InternalError // should never happen (but will)
)

type Diagnostics []*Diagnostic

func (this Diagnostics) String() string {
	output := ""
	for _, diag := range this {
		output += diag.String() + "\n------------\n"
	}
	return output
}

type Diagnostic struct {
	Location Location
	Severity Severity
	Source   string
	Message  string
	Code     dc.DiagnosticCode
}

func (this *Diagnostic) String() string {
	return this.Location.String() + " " +
		this.Severity.String() +
		": " + this.Message
}

func NewInternalError(message string) *Diagnostic {
	return &Diagnostic{
		Message:  message,
		Severity: InternalError,
		Code:     dc.InternalCompilerError,
	}
}
