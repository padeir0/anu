package mdir

import (
	. "abc/core"
	c "abc/core/mdir/class"
	it "abc/core/mdir/instrtype"
	T "abc/core/planartypes"
	"strconv"
)

type Program struct {
	Symbols []*Symbol
}

func (this *Program) FindSymbol(i int64) (*Symbol, *Diagnostic) {
	if i < 0 || i >= int64(len(this.Symbols)) {
		return nil, NewInternalError("Index out of bounds for Symbols Array")
	}
	sy := this.Symbols[i]
	if sy == nil {
		return nil, NewInternalError("Symbol not found in program: " + strconv.FormatInt(i, 10))
	}
	return sy, nil
}

// Symbol = Procedure | Memory
// but since Go doesn't have sums
type Symbol struct {
	Proc *Proc
	Mem  *Mem
}

type Proc struct {
	Label string
	Type  *T.ProcType
	Start *BasicBlock
}

func (p *Proc) ResetVisited() {
	resetVisitedBB(p.Start)
}

func resetVisitedBB(bb *BasicBlock) {
	if !bb.Visited {
		return
	}
	bb.Visited = false
	switch bb.Out.T {
	case If:
		resetVisitedBB(bb.Out.True)
		resetVisitedBB(bb.Out.False)
	case Return:
		return
	case Jmp:
		resetVisitedBB(bb.Out.True)
	}
}

// Mem = Reserved | Declared
type Mem struct {
	Label string
	Data  string
	Type  *T.Type
	Size  int
}

type BasicBlock struct {
	Label   string
	Code    []*Instr
	Out     Flow
	Visited bool
}

type Instr struct {
	T    it.InstrType
	Type T.BasicType // type operand (casts)
	A    *Operand
	B    *Operand
	Dest *Operand
}

func (this *Instr) String() string {
	output := this.T.String()
	if T.IsValid(this.Type) {
		output += ":" + this.Type.String()
	}
	if this.A != nil {
		output += " " + this.A.String()
		if this.B != nil {
			output += " " + this.B.String()
		}
	} else {
		if this.B != nil {
			output += " ???, " + this.B.String()
		}
	}
	if this.Dest != nil {
		output += " -> " + this.Dest.String()
	}
	return output
}

type Operand struct {
	Class c.Class
	Type  T.Type
	Num   int64
}

func (this *Operand) String() string {
	s := strconv.FormatInt(this.Num, 10)
	switch this.Class {
	case c.Register:
		return "r" + s
	case c.Spill:
		return "s" + s
	case c.CallerInterproc:
		return "top" + s
	case c.CalleeInterproc:
		return "bot" + s
	case c.Local:
		return "l" + s
	case c.Lit:
		return s
	case c.Static:
		return "static#" + s
	}
	panic("invalid class")
}

type Flow struct {
	T     FlowType
	V     *Operand
	True  *BasicBlock
	False *BasicBlock
}

func (this *Flow) String() string {
	switch this.T {
	case Jmp:
		return "jmp " + this.True.Label
	case If:
		return "if " + this.V.String() + "? " + this.True.Label + " : " + this.False.Label
	case Return:
		return "ret " + this.V.String()
	case Exit:
		return "exit " + this.V.String()
	}
	return "invalid FlowType"
}

type FlowType int

func (this FlowType) String() string {
	switch this {
	case Jmp:
		return "jmp"
	case If:
		return "if"
	case Return:
		return "ret"
	case Exit:
		return "exit"
	}
	return "invalid FlowType"
}

const (
	InvalidFlow FlowType = iota

	Jmp
	If
	Return
	Exit
)
