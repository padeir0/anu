package typechecker

import (
	. "abc/core"
	mod "abc/core/module"
	T "abc/core/types"

	lk "abc/core/lexeme/lexkind"
	nk "abc/core/module/nodekind"
)

func Check(m *mod.Module) *Error {
	m.ResetVisited()
	tspace := T.NewTypeSpace()
	return check(m, tspace)
}

// we have to typecheck in order
//     - First we typecheck all our dependencies
//     - Then we evaluate type declarations, creating each type in the typespace.
//     This happens separatedly because constants and procedures depend on types.
//     - Then we infer and check global constants and procedures, but we only
//     look at the signature of procedures. We do this in a separate step to allow
//     out of order symbols.
//     - At this point all global information is typechecked and we can check
//     procedure expressions.
func check(m *mod.Module, tspace *T.TypeSpace) *Error {
	if m.Visited {
		return nil
	}
	m.Visited = true
	for _, dep := range m.Dependencies {
		err := check(dep.M, tspace)
		if err != nil {
			return err
		}
	}

	err := createGlobalTypes(m, tspace)
	if err != nil {
		return err
	}

	err = inferGlobalSymbols(m, tspace)
	if err != nil {
		return err
	}

	err = checkProcExpressions(m, tspace)
	if err != nil {
		return err
	}

	return nil
}

func checkProcExpressions(m *mod.Module, tspace *T.TypeSpace) *Error {
	panic("unimplemented")
}

func inferGlobalSymbols(m *mod.Module, tspace *T.TypeSpace) *Error {
	panic("unimplemented")
}

// create all types in **topological** order, following the reference graph
func createGlobalTypes(m *mod.Module, tspace *T.TypeSpace) *Error {
	panic("unimplemented")
}

func createType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	if n == nil {
		return T.InvalidTypeID, nil
	}
	switch n.Kind {
	case nk.Sum:
		return createSumType(M, scope, n)
	case nk.Product:
		return createProductType(M, scope, n)
	case nk.Terminal:
		switch n.Lexeme.Kind {
		case lk.Arrow:
			return createMapType(M, scope, n)
		case lk.Star:
			return createArrayType(M, scope, n)
		case lk.Ampersand:
			return createRefType(M, scope, n)
		case lk.Proc:
			return createProcType(M, scope, n)
		case lk.QuestionMark:
			return createOptType(M, scope, n)
		case lk.DoubleColon:
		case lk.Ident:
		}
	}
	panic("unreachable")
}

func createSumType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createProductType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createMapType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createArrayType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createRefType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createProcType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
func createOptType(M *mod.Module, scope *mod.Scope, n *mod.Node) (T.TypeID, *Error) {
	panic("unimplemented")
}
