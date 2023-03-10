package typechecker

import (
	. "abc/core"
	mod "abc/core/module"
	T "abc/core/types"
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
