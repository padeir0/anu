package mdir

import (
	c "abc/backend/mdir/class"
	it "abc/backend/mdir/instrtype"
	T "abc/backend/mdir/types"
	"strconv"
)

type Program struct {
	Static []*Symbol
}

// Symbol = Procedure | Memory
// but since Go doesn't have sums
type Symbol struct {
	Label string
	Proc  *Proc
	Mem   *Mem
}

type Proc struct {
	Arguments []T.Type
	Returns   []T.Type
	Locals    []T.Type
	Start     *BasicBlock
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
	Data string
	Size int
}

type BasicBlock struct {
	Label   string
	Code    []*Instr
	Out     Flow
	Visited bool
}

type Instr struct {
	T    it.InstrType
	Type T.Type
	A    *Operand
	B    *Operand
	Dest *Operand
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
