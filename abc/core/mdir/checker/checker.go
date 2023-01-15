package checker

import (
	. "abc/core"
	ir "abc/core/mdir"
	c "abc/core/mdir/class"
	IT "abc/core/mdir/instrtype"
	T "abc/core/types"

	"strings"
)

func Check(M *ir.Program) *Diagnostic {
	for _, sy := range M.Symbols {
		if sy.Proc != nil {
			s := newState(M)
			s.proc = sy.Proc
			s.proc.ResetVisited()
			s.Init()
			err := checkCode(s, sy.Proc.Start)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

type region []*ir.Operand

func newRegion(size int64) region {
	return make(region, size)
}

func (r *region) String() string {
	output := []string{}
	for _, op := range *r {
		output = append(output, op.String())
	}
	return strings.Join(output, ", ")
}

func (r *region) Store(i int64, op *ir.Operand) {
	if i >= int64(len(*r)) {
		*r = append(*r, newRegion(i-int64(len(*r)+1))...)
	}
	(*r)[i] = op
}

func (r *region) Load(i int64) *ir.Operand {
	if i >= int64(len(*r)) {
		return nil
	}
	return (*r)[i]
}

func (r region) Clear(i int) {
	if i >= len(r) {
		return
	}
	r[i] = nil
}

type state struct {
	program *ir.Program
	proc    *ir.Proc
	bb      *ir.BasicBlock

	CalleeInterproc region
	CallerInterproc region
	Spill           region
	Registers       region
	Locals          region
}

func newState(P *ir.Program) *state {
	return &state{
		program:         P,
		CalleeInterproc: newRegion(8),
		CallerInterproc: newRegion(8),
		Spill:           newRegion(8),
		Registers:       newRegion(8),
		Locals:          newRegion(8),
	}
}

func (s *state) Init() {
	for i, arg := range s.proc.Arguments {
		argOp := newCallerOperand(int64(i), arg)
		s.CallerInterproc.Store(int64(i), argOp)
	}
	for i, loc := range s.proc.Locals {
		locOp := newCallerOperand(int64(i), loc)
		s.Locals.Store(int64(i), locOp)
	}
}

func (s *state) String() string {
	return s.proc.Label + s.bb.Label + "\n" +
		"callee: " + s.CalleeInterproc.String() + "\n" +
		"caller: " + s.CallerInterproc.String() + "\n" +
		"spill: " + s.Spill.String() + "\n" +
		"registers: " + s.Registers.String() + "\n" +
		"locals: " + s.Locals.String() + "\n"
}

func (s *state) Copy() *state {
	caller := make(region, len(s.CallerInterproc))
	callee := make(region, len(s.CalleeInterproc))
	spill := make(region, len(s.Spill))
	registers := make(region, len(s.Registers))
	locals := make(region, len(s.Locals))
	copy(caller, s.CallerInterproc)
	copy(callee, s.CalleeInterproc)
	copy(spill, s.Spill)
	copy(registers, s.Registers)
	copy(locals, s.Locals)
	return &state{
		CallerInterproc: caller,
		CalleeInterproc: callee,
		Spill:           spill,
		Registers:       registers,
		Locals:          locals,
		bb:              s.bb,
		program:         s.program,
		proc:            s.proc,
	}
}

func (s *state) SetReg(op *ir.Operand) {
	if op.Class != c.Register {
		panic("is not setting a register: " + op.String())
	}
	s.Registers.Store(op.Num, op)
}

func newCallerOperand(i int64, t T.TypeID) *ir.Operand {
	return &ir.Operand{
		Class: c.CallerInterproc,
		Num:   i,
		Type:  t,
	}
}

func newLocalOperand(i int64, t T.TypeID) *ir.Operand {
	return &ir.Operand{
		Class: c.Local,
		Num:   i,
		Type:  t,
	}
}

func checkCode(s *state, bb *ir.BasicBlock) *Diagnostic {
	if bb.Visited {
		return nil
	}
	s.bb = bb
	for _, instr := range bb.Code {
		err := checkInstr(s, instr)
		if err != nil {
			return err
		}
	}
	bb.Visited = true
	return checkJump(s)
}

func checkJump(s *state) *Diagnostic {
	bb := s.bb
	switch bb.Out.T {
	case ir.Jmp:
		return checkCode(s, bb.Out.True)
	case ir.If:
		s2 := s.Copy()
		err := checkCode(s, bb.Out.True)
		if err != nil {
			return err
		}
		return checkCode(s2, bb.Out.False)
	case ir.Return:
		return checkRet(s)
	}
	return nil
}

func checkRet(s *state) *Diagnostic {
	for i, ret := range s.proc.Returns {
		op := s.CallerInterproc.Load(int64(i))
		if op == nil {
			return NewInternalError("return stack is empty, expected returns: " + s.proc.StrRets())
		}
		if ret != op.Type {
			return NewInternalError("return of type " + ret.String() + " doesn't match value in stack: " + s.CallerInterproc.String())
		}
		s.CallerInterproc.Clear(i)
	}
	return nil
}

type Checker struct {
	Class func(c.Class) bool
	Type  func(T.TypeID) bool
}

func (c *Checker) Check(op *ir.Operand) bool {
	return c.Type(op.Type) && c.Class(op.Class)
}

var basicOrProc_imme = Checker{
	Class: c.IsImmediate,
	Type:  T.IsValid,
}

var basicOrProc_reg = Checker{
	Class: c.IsRegister,
	Type:  T.IsValid,
}

var basicOrProc_addr = Checker{
	Class: c.IsAddressable,
	Type:  T.IsValid,
}

var castable_imme = Checker{
	Class: c.IsImmediate,
	Type:  T.IsCastable,
}

var castable_reg = Checker{
	Class: c.IsRegister,
	Type:  T.IsCastable,
}

var num_imme = Checker{
	Class: c.IsImmediate,
	Type:  T.IsNumber,
}

var num_reg = Checker{
	Class: c.IsRegister,
	Type:  T.IsNumber,
}

var bool_imme = Checker{
	Class: c.IsImmediate,
	Type:  T.IsBool,
}

var bool_reg = Checker{
	Class: c.IsRegister,
	Type:  T.IsBool,
}

var ptr_imme = Checker{
	Class: c.IsImmediate,
	Type:  T.IsPtr,
}

var ptr_reg = Checker{
	Class: c.IsRegister,
	Type:  T.IsPtr,
}

func checkInstr(s *state, instr *ir.Instr) *Diagnostic {
	if instr == nil {
		return nilInstr(s)
	}
	err := checkInvalidClass(instr)
	if err != nil {
		return err
	}
	switch instr.T {
	case IT.Add, IT.Sub, IT.Div, IT.Mult, IT.Rem:
		return checkArith(s, instr)
	case IT.Eq, IT.Diff, IT.Less, IT.More, IT.LessEq, IT.MoreEq:
		return checkComp(s, instr)
	case IT.Or, IT.And:
		return checkLogical(s, instr)
	case IT.Not:
		return checkNot(s, instr)
	case IT.Neg:
		return checkUnaryArith(s, instr)
	case IT.Convert:
		return checkConvert(s, instr)
	case IT.LoadPtr:
		return checkLoadPtr(s, instr)
	case IT.StorePtr:
		return checkStorePtr(s, instr)
	case IT.Store:
		return checkStore(s, instr)
	case IT.Load:
		return checkLoad(s, instr)
	case IT.Copy:
		return checkCopy(s, instr)
	case IT.Call:
		return checkCall(s, instr)
	}
	panic("sumthin' went wong")
}

func checkArith(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, num_imme, num_imme, num_reg)
}

func checkComp(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, castable_imme, castable_imme, bool_reg)
}

func checkLogical(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, true, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.B.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkBinary(instr, bool_imme, bool_imme, bool_reg)
}

func checkUnaryArith(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, num_imme, num_reg)
}

func checkNot(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, bool_imme, bool_reg)
}

func checkConvert(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, castable_imme, castable_reg)
}

func checkLoadPtr(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	return checkUnary(instr, ptr_imme, basicOrProc_reg)
}

func checkStorePtr(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, true, false)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, instr.A.Type)
	if err != nil {
		return err
	}
	if basicOrProc_imme.Check(instr.A) &&
		ptr_imme.Check(instr.Dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkLoad(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_addr, basicOrProc_reg)
	if err != nil {
		return err
	}

	return checkLoadState(s, instr)
}

func checkStore(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_addr)
	if err != nil {
		return err
	}

	return checkStoreState(s, instr)
}

func checkCopy(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, true)
	if err != nil {
		return err
	}

	err = checkRegs(s, instr)
	if err != nil {
		return err
	}
	s.SetReg(instr.Dest)

	err = checkEqual(instr, instr.Type, instr.A.Type, instr.Dest.Type)
	if err != nil {
		return err
	}
	err = checkUnary(instr, basicOrProc_imme, basicOrProc_reg)
	if err != nil {
		return err
	}

	return nil
}

func checkCall(s *state, instr *ir.Instr) *Diagnostic {
	err := checkForm(instr, true, false, false)
	if err != nil {
		return err
	}

	sy, err := s.program.FindSymbol(instr.A.Num)
	if err != nil {
		return err
	}

	if sy.Proc == nil {
		return notAProc(instr)
	}

	for i, formal_arg := range sy.Proc.Arguments {
		real_arg := s.CalleeInterproc.Load(int64(i))
		if real_arg == nil {
			return errorCallLoadingGarbage(instr)
		}
		if formal_arg != real_arg.Type {
			return procBadArg(instr, formal_arg, real_arg)
		}
		s.CalleeInterproc.Clear(i)
	}

	for i, formal_ret := range sy.Proc.Returns {
		op := &ir.Operand{Class: c.CalleeInterproc, Num: int64(i), Type: formal_ret}
		s.CalleeInterproc.Store(int64(i), op)
	}
	return nil
}

func checkLoadState(s *state, instr *ir.Instr) *Diagnostic {
	var source *ir.Operand
	switch instr.A.Class {
	case c.Spill:
		source = s.Spill.Load(instr.A.Num)
	case c.CalleeInterproc:
		source = s.CalleeInterproc.Load(instr.A.Num)
	case c.CallerInterproc:
		source = s.CallerInterproc.Load(instr.A.Num)
	case c.Local:
		source = s.Locals.Load(instr.A.Num)
	default:
		panic("oh no")
	}
	if source == nil {
		return errorLoadingGarbage(instr)
	}
	err := checkEqual(instr, instr.Dest.Type, source.Type)
	if err != nil {
		return err
	}
	return nil
}

func checkStoreState(s *state, instr *ir.Instr) *Diagnostic {
	switch instr.Dest.Class {
	case c.Spill:
		s.Spill.Store(instr.Dest.Num, instr.A)
	case c.CalleeInterproc:
		s.CalleeInterproc.Store(instr.Dest.Num, instr.A)
	case c.CallerInterproc:
		s.CallerInterproc.Store(instr.Dest.Num, instr.A)
	case c.Local:
		s.Locals.Store(instr.Dest.Num, instr.A)
	default:
		panic("oh no")
	}
	return nil
}

func checkRegs(s *state, instr *ir.Instr) *Diagnostic {
	err := checkRegOperand(s, instr, instr.A)
	if err != nil {
		return err
	}
	err = checkRegOperand(s, instr, instr.B)
	if err != nil {
		return err
	}
	return nil
}

func checkRegOperand(s *state, instr *ir.Instr, op *ir.Operand) *Diagnostic {
	if op == nil {
		return nil
	}
	if op.Class == c.Register {
		loaded := s.Registers.Load(op.Num)
		if loaded == nil {
			return errorUsingRegisterGarbage(instr, op)
		}
		if loaded.Num != op.Num || loaded.Class != op.Class || loaded.Type != op.Type {
			return errorIncorrectValueInRegister(instr, loaded, op)
		}
	}
	return nil
}

func checkEqual(instr *ir.Instr, types ...T.Type) *Diagnostic {
	if len(types) == 0 {
		return nil
	}
	first := types[0]
	for _, t := range types[1:] {
		if first != t {
			return malformedEqualTypes(instr)
		}
	}
	return nil
}

func checkBinary(instr *ir.Instr, checkA, checkB, checkC Checker) *Diagnostic {
	if checkA.Check(instr.A) &&
		checkB.Check(instr.B) &&
		checkC.Check(instr.Dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkUnary(instr *ir.Instr, checkA, checkC Checker) *Diagnostic {
	if checkA.Check(instr.A) &&
		checkC.Check(instr.Dest) {
		return nil
	}
	return malformedTypeOrClass(instr)
}

func checkInvalidClass(instr *ir.Instr) *Diagnostic {
	if instr.A != nil && instr.A.Class == c.InvalidMIRClass {
		return invalidClass(instr)
	}
	if instr.B != nil && instr.B.Class == c.InvalidMIRClass {
		return invalidClass(instr)
	}
	if instr.Dest != nil && instr.Dest.Class == c.InvalidMIRClass {
		return invalidClass(instr)
	}
	return nil
}

func checkForm(instr *ir.Instr, hasA, hasB, hasDest bool) *Diagnostic {
	if hasA && instr.A == nil {
		return malformedInstr(instr)
	}
	if hasB && instr.B == nil {
		return malformedInstr(instr)
	}
	if hasDest && instr.Dest == nil {
		return malformedInstr(instr)
	}
	return nil
}

func malformedInstr(instr *ir.Instr) *Diagnostic {
	return NewInternalError("malformed instruction: " + instr.String())
}
func malformedEqualTypes(instr *ir.Instr) *Diagnostic {
	return NewInternalError("unequal types: " + instr.String())
}
func malformedTypeOrClass(instr *ir.Instr) *Diagnostic {
	return NewInternalError("malformed type or class: " + instr.String())
}
func invalidClass(instr *ir.Instr) *Diagnostic {
	return NewInternalError("invalid class: " + instr.String())
}
func errorLoadingGarbage(instr *ir.Instr) *Diagnostic {
	return NewInternalError("loading garbage: " + instr.String())
}
func errorCallLoadingGarbage(instr *ir.Instr) *Diagnostic {
	return NewInternalError("call loading garbage: " + instr.String())
}
func errorUsingRegisterGarbage(instr *ir.Instr, op *ir.Operand) *Diagnostic {
	return NewInternalError("using register garbage: " + op.String() + " of " + instr.String())
}
func errorIncorrectValueInRegister(instr *ir.Instr, o, op *ir.Operand) *Diagnostic {
	return NewInternalError("incorrect value in register (" + o.String() + "): " + op.String() + " of " + instr.String())
}
func errorLoadingIncorrectType(instr *ir.Instr) *Diagnostic {
	return NewInternalError("load of incorrect type: " + instr.String())
}
func notAProc(instr *ir.Instr) *Diagnostic {
	return NewInternalError("not a procedure: " + instr.String())
}
func procArgNotFound(instr *ir.Instr, proc *ir.Proc) *Diagnostic {
	return NewInternalError("argument " + proc.Label + " not found in: " + instr.String())
}
func procBadArg(instr *ir.Instr, d T.Type, op *ir.Operand) *Diagnostic {
	return NewInternalError("argument " + op.String() + " doesn't match formal parameter (" + d.String() + ") in: " + instr.String())
}
func procBadRet(instr *ir.Instr, d T.Type, op *ir.Operand) *Diagnostic {
	return NewInternalError("return " + op.String() + " doesn't match formal return " + d.String() + " in: " + instr.String())
}

func nilInstr(s *state) *Diagnostic {
	return NewInternalError("nil instruction in: " + s.proc.Label + " " + s.bb.Label)
}
