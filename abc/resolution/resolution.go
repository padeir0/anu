package resolution

import (
	. "abc/core"
	ir "abc/core/module"
	"abc/lexer"
	"abc/parser"

	et "abc/core/errorkind"
	lk "abc/core/lexeme/lexkind"
	nk "abc/core/module/nodekind"
	sk "abc/core/module/symbolkind"
	sv "abc/core/severity"

	"fmt"
	"io/ioutil"
	"strings"
)

func Resolve(filePath string) (*ir.Module, *Error) {
	name, err := extractName(filePath)
	if err != nil {
		return nil, err
	}

	s, ioerr := newState(filePath)
	if ioerr != nil {
		return nil, ProcessFileError(ioerr)
	}

	m, err := resolveModule(s, name)
	if err != nil {
		return nil, err
	}
	err = checkDependencyCycles(m)
	if err != nil {
		return nil, err
	}
	err = resolveNames(m)
	if err != nil {
		return nil, err
	}
	return m, nil
}

func resolveNames(M *ir.Module) *Error {
	err := resolve(M)
	if err != nil {
		return err
	}
	M.ResetVisited()
	return nil
}

// we resolve modules one by one, beginning
// with modules that have no dependencies (leafs)
// and going up
// a module is only resolved when all it's dependencies
// have finished resolving and returned no errors
func resolve(M *ir.Module) *Error {
	if M.Visited {
		return nil
	}
	M.Visited = true
	for _, dep := range M.Dependencies {
		err := resolve(dep.M)
		if err != nil {
			return err
		}
	}

	err := createImportedSymbols(M)
	if err != nil {
		return err
	}

	err = createGlobals(M)
	if err != nil {
		return err
	}

	err = resolveGlobalSymbolsReferences(M)
	if err != nil {
		return err
	}

	err = checkValCycles(M)
	if err != nil {
		return err
	}

	err = checkAllTypeCycles(M)
	if err != nil {
		return err
	}

	err = checkExports(M)
	if err != nil {
		return err
	}

	//TODO: IMPROVEMENT: maybe make this return multiple errors later
	err = resolveProcScopes(M)
	if err != nil {
		return err
	}

	return nil
}

func resolveProcScopes(M *ir.Module) *Error {
	for _, sy := range M.Global.Symbols {
		if sy.External {
			continue
		}
		switch sy.Kind {
		case sk.Procedure:
			err := resolveInnerProc(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resolveInnerProc(M *ir.Module, sy *ir.Symbol) *Error {
	argScope := &ir.Scope{
		Parent:  M.Global,
		Symbols: map[string]*ir.Symbol{},
	}
	sy.N.Scope = argScope
	// declaring arguments
	sig := sy.N.Leaves[2]
	if sig != nil {
		args := sig.Leaves[0]
		for _, arg := range args.Leaves {
			var name *ir.Node
			if arg.Lexeme.Kind == lk.Colon {
				name = arg.Leaves[0]
			} else {
				name = arg
			}
			symbol := &ir.Symbol{
				Kind:     sk.Argument,
				Name:     name.Lexeme.Text,
				N:        name,
				M:        M,
				External: false,
			}
			argScope.Add(name.Lexeme.Text, symbol)
		}
	}
	expr := sy.N.Leaves[3]
	return resolveExpr(M, sy, argScope, expr, false)
}

func resolveGlobalSymbolsReferences(M *ir.Module) *Error {
	for _, sy := range M.Global.Symbols {
		if sy.External {
			continue
		}
		switch sy.Kind {
		case sk.TypeAlias:
			err := resolveTypeAlias(M, sy)
			if err != nil {
				return err
			}
		case sk.TypeEnumOption:
			err := resolveTypeEnumOption(M, sy)
			if err != nil {
				return err
			}
		case sk.TypeEnum:
			err := resolveTypeEnum(M, sy)
			if err != nil {
				return err
			}
		case sk.TypeCreation:
			err := resolveTypeCreation(M, sy)
			if err != nil {
				return err
			}
		case sk.Procedure:
			err := resolveProc(M, sy)
			if err != nil {
				return err
			}
		case sk.Constant:
			err := resolveConst(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resolveConst(M *ir.Module, sy *ir.Symbol) *Error {
	annot := sy.N.Leaves[1]
	exp := sy.N.Leaves[2]
	if annot != nil {
		texp := annot.Leaves[0]
		err := resolveTypeExpr(M, sy, M.Global, false, false, texp)
		if err != nil {
			return err
		}
	}
	return resolveExpr(M, sy, M.Global, exp, true)
}

func resolveTypeEnum(M *ir.Module, sy *ir.Symbol) *Error {
	def := sy.N.Leaves[1]
	for _, leaf := range def.Leaves {
		var name string
		if leaf.Lexeme.Kind == lk.Is {
			name = leaf.Leaves[0].Lexeme.Text
		} else {
			name = leaf.Lexeme.Text
		}
		optionSymbol := M.Global.Find(name)
		if optionSymbol == nil {
			panic("invalid undeclared option on enum")
		}
		sy.Link(optionSymbol)
	}
	return nil
}

func resolveTypeEnumOption(M *ir.Module, sy *ir.Symbol) *Error {
	if sy.N.Lexeme.Kind == lk.Is {
		texp := sy.N.Leaves[1]
		// recursion is allowed
		return resolveTypeExpr(M, sy, M.Global, false, true, texp)
	}
	return nil
}

func resolveTypeCreation(M *ir.Module, sy *ir.Symbol) *Error {
	def := sy.N.Leaves[1]
	texp := def.Leaves[0]
	// recursion is allowed
	return resolveTypeExpr(M, sy, M.Global, false, true, texp)
}

func resolveTypeAlias(M *ir.Module, sy *ir.Symbol) *Error {
	def := sy.N.Leaves[1]
	texp := def.Leaves[0]
	// recursion is not allowed
	return resolveTypeExpr(M, sy, M.Global, false, false, texp)
}

func resolveProc(M *ir.Module, sy *ir.Symbol) *Error {
	annot := sy.N.Leaves[1]
	sig := sy.N.Leaves[2]

	if annot != nil {
		typeexp := annot.Leaves[0]
		// recursion is impossible on anonimous types
		err := resolveTypeExpr(M, sy, M.Global, false, false, typeexp)
		if err != nil {
			return err
		}
	}

	if sig != nil {
		args := sig.Leaves[0]
		if args != nil {
			for _, leaf := range args.Leaves {
				if leaf.Lexeme.Kind == lk.Colon {
					typeexpr := leaf.Leaves[1]
					err := resolveTypeExpr(M, sy, M.Global, false, false, typeexpr)
					if err != nil {
						return err
					}
				}
			}
		}
		ret := sig.Leaves[1]
		if ret != nil {
			err := resolveTypeExpr(M, sy, M.Global, false, false, ret)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

// resolves names for expressions, creating a dependency graph
// between symbols
// it works for procedure bodies and global constants
//     - 'scope' is the current scope, starting with global and descending
//     when scopes are created
//     - 'isConst' tells whether we're in a constant expression or not
//     so we can raise errors when we encounter non-constant expressions
//     and otherwise ignore arbitrary recursion
//
// we will ignore product field accesses and named procedure arguments,
// deferring it to typechecking,
// as both are essentially auxiliary type information
// that is bound to procedures and products
func resolveExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	switch n.Kind {
	case nk.Terminal:
		switch n.Lexeme.Kind {
		case lk.IntLit, lk.I8Lit, lk.I16Lit, lk.I32Lit, lk.I64Lit,
			lk.True, lk.False, lk.StringLit, lk.Nil, lk.CharLit:
			// nothing to resolve here
			return nil
		case lk.DoubleColon:
			// modules can't be recursive, so expressions
			// can't be recursive either
			return nil
		// binary operations
		case lk.Or, lk.And, lk.Equals, lk.Different,
			lk.Greater, lk.Less, lk.GreaterOrEquals, lk.LessOrEquals,
			lk.Plus, lk.Minus, lk.DoubleDot, lk.Division, lk.Star,
			lk.Remainder:
			err := resolveExpr(M, sy, scope, n.Leaves[0], isConst)
			if err != nil {
				return err
			}
			return resolveExpr(M, sy, scope, n.Leaves[1], isConst)
		// unary operations
		case lk.Not, lk.Tilde, lk.Dollar:
			return resolveExpr(M, sy, scope, n.Leaves[0], isConst)
		// special cases (expr <op> type)
		case lk.Is, lk.Colon, lk.Caret:
			err := resolveExpr(M, sy, scope, n.Leaves[0], isConst)
			if err != nil {
				return err
			}
			return resolveTypeExpr(M, sy, scope, false, false, n.Leaves[1])
		// only consider left side
		// validation of field names is deferred to typechecking
		case lk.Dot:
			return resolveExpr(M, sy, scope, n.Leaves[0], isConst)

		// atoms
		case lk.Ident:
			// we will need a way to know whether a found symbol is global
			// or not
			return resolveIdentExpr(M, sy, scope, n, isConst)
		// rather useless on constants but valid nevertheless
		case lk.New:
			return resolveNewExpr(M, sy, scope, n, isConst)
		case lk.Let:
			return resolveLetExpr(M, sy, scope, n, isConst)
		// not const expression
		case lk.Ampersand:
			if isConst {
				return errorInvalidConstExpr(M, n, "& (address-of)")
			}
			return resolveExpr(M, sy, scope, n.Leaves[0], isConst)
		case lk.At:
			if isConst {
				return errorInvalidConstExpr(M, n, "@ (value-at)")
			}
			return resolveExpr(M, sy, scope, n.Leaves[0], isConst)
		case lk.For:
			if isConst {
				return errorInvalidConstExpr(M, n, "for loops")
			}
			return resolveForExpr(M, sy, scope, n)
		case lk.While:
			if isConst {
				return errorInvalidConstExpr(M, n, "while loops")
			}
			return resolveWhileExpr(M, sy, scope, n)
		case lk.Range:
			if isConst {
				return errorInvalidConstExpr(M, n, "range loops")
			}
			return resolveRangeExpr(M, sy, scope, n)
		case lk.Return:
			if isConst {
				return errorInvalidConstExpr(M, n, "returns")
			}
			return resolveReturnExpr(M, sy, scope, n)
		case lk.Set:
			if isConst {
				return errorInvalidConstExpr(M, n, "assignments")
			}
			return resolveSetExpr(M, sy, scope, n)
		// this might be too agressive
		case lk.If:
			if isConst {
				return errorInvalidConstExpr(M, n, "ifs")
			}
			return resolveIfExpr(M, sy, scope, n)
		case lk.Switch:
			if isConst {
				return errorInvalidConstExpr(M, n, "switches")
			}
			return resolveSwitchExpr(M, sy, scope, n)
		case lk.Match:
			if isConst {
				return errorInvalidConstExpr(M, n, "switches")
			}
			return resolveMatchExpr(M, sy, scope, n)
		}
	case nk.CallOrIndex:
		if isConst {
			return errorInvalidConstExpr(M, n, "function calls/indexings/lookups")
		}
		return resolveCallOrIndexExpr(M, sy, scope, n)
	case nk.Block:
		if isConst {
			return errorInvalidConstExpr(M, n, "code blocks")
		}
		return resolveBlockExpr(M, sy, scope, n)
	case nk.ArrayMapLit:
		return resolveArrayMapLitExpr(M, sy, scope, n, isConst)
	case nk.ProductLit:
		return resolveProductLitExpr(M, sy, scope, n, isConst)
	}

	return nil
}

func resolveReturnExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	if len(n.Leaves) == 1 && n.Leaves[0] != nil {
		return resolveExpr(M, sy, scope, n.Leaves[0], false)
	}
	return nil
}

func resolveIdentExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	name := n.Lexeme.Text
	otherSy := scope.Find(name)
	if otherSy == nil {
		return errorSymbolNotDeclared(M, n)
	}
	// we don't need to check for cycles in non-constant expressions,
	// they can't form cycles because they must be declared in order
	if isConst {
		sy.Link(otherSy)
	}
	return nil
}

// const global0 = let a = 0  in ...
// ---------0--^       ^--1---------
//
// const global1 = let a = 0, b = 1, c = 2 in ...
// -------0----^       ^---------1----------------
//
// proc main do begin let a = 0, b = 1 end
// --0-----^    ^-----------1------------^
func resolveLetExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	decls := n.Leaves[0]
	expr := n.Leaves[1]
	if expr != nil {
		newScope := &ir.Scope{
			Parent:  scope,
			Symbols: map[string]*ir.Symbol{},
		}
		n.Scope = scope
		scope = newScope
	}
	for _, subDecls := range decls.Leaves {
		decls := subDecls.Leaves[0]
		exp := subDecls.Leaves[1]
		err := resolveExpr(M, sy, scope, exp, isConst)
		if err != nil {
			return err
		}
		for _, decl := range decls.Leaves {
			var name *ir.Node
			if decl.Lexeme.Kind == lk.Colon {
				name = decl.Leaves[0]
				texp := decl.Leaves[1]
				err := resolveTypeExpr(M, sy, scope, false, false, texp)
				if err != nil {
					return err
				}
			} else {
				name = decl
			}
			newSymbol := &ir.Symbol{
				Kind:     sk.Local,
				Name:     name.Lexeme.Text,
				N:        name,
				M:        M,
				External: false,
			}
			scope.Add(name.Lexeme.Text, newSymbol)
		}
	}
	if expr != nil {
		return resolveExpr(M, sy, scope, expr, isConst)
	}
	return nil
}

func resolveSetExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	lefties := n.Leaves[0]
	right := n.Leaves[1]

	for _, left := range lefties.Leaves {
		err := resolveExpr(M, sy, scope, left, false)
		if err != nil {
			return err
		}
	}
	return resolveExpr(M, sy, scope, right, false)
}

func resolveNewExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	annot := n.Leaves[0]
	err := resolveTypeExpr(M, sy, scope, false, false, annot)
	if err != nil {
		return err
	}
	for _, leaf := range n.Leaves {
		var expr *ir.Node
		if leaf.Lexeme.Kind == lk.Assign {
			field := n.Leaves[0]
			// validation of field names is deferred to typechecking
			if field.Lexeme.Kind != lk.Ident {
				return errorInvalidField(M, field)
			}
			expr = n.Leaves[1]
		} else {
			expr = n
		}
		err := resolveExpr(M, sy, scope, expr, isConst)
		if err != nil {
			return err
		}
	}
	return nil
}

func resolveWhileExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	cond := n.Leaves[0]
	err := resolveExpr(M, sy, scope, cond, false)
	if err != nil {
		return err
	}
	resExpr := n.Leaves[1]
	return resolveExpr(M, sy, scope, resExpr, false)
}

// -0---v                                       v--1--v
// proc A[array:*int] do for index, item in array do ...;
//       ^-----1------------ ^----2----^             ^--2---
func resolveForExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	collection := n.Leaves[2]
	err := resolveExpr(M, sy, scope, collection, false)
	if err != nil {
		return err
	}

	newScope := &ir.Scope{
		Parent:  scope,
		Symbols: map[string]*ir.Symbol{},
	}
	{
		id := n.Leaves[0]
		idSymbol := &ir.Symbol{
			Kind:     sk.Local,
			Name:     id.Lexeme.Text,
			N:        id,
			M:        M,
			Refs:     nil,
			External: false,
		}
		newScope.Add(id.Lexeme.Text, idSymbol)
	}
	{
		id := n.Leaves[1]
		if id != nil {
			idSymbol := &ir.Symbol{
				Kind:     sk.Local,
				Name:     id.Lexeme.Text,
				N:        id,
				M:        M,
				Refs:     nil,
				External: false,
			}
			newScope.Add(id.Lexeme.Text, idSymbol)
		}
	}
	n.Scope = newScope
	resExpr := n.Leaves[3]
	return resolveExpr(M, sy, newScope, resExpr, false)
}

// -0---v
// proc A[array:*int] do range 0 to 10 as  i  do ...;
//       ^----1----------------------^   ^-2--------
func resolveRangeExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	initial := n.Leaves[0]
	err := resolveExpr(M, sy, scope, initial, false)
	if err != nil {
		return err
	}

	target := n.Leaves[1]
	err = resolveExpr(M, sy, scope, target, false)
	if err != nil {
		return err
	}

	as := n.Leaves[2]
	if as != nil {
		newScope := &ir.Scope{
			Parent:  scope,
			Symbols: map[string]*ir.Symbol{},
		}
		sy := &ir.Symbol{
			Kind:     sk.Local,
			Name:     as.Lexeme.Text,
			N:        as,
			M:        M,
			External: false,
		}
		newScope.Add(as.Lexeme.Text, sy)
		scope = newScope
		as.Scope = newScope
	}
	// only create new scope if `as` is present
	resExpr := n.Leaves[3]
	return resolveExpr(M, sy, scope, resExpr, false)
}

func resolveIfExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	cond := n.Leaves[0]
	err := resolveExpr(M, sy, scope, cond, false)
	if err != nil {
		return err
	}
	firstResExpr := n.Leaves[1]
	err = resolveExpr(M, sy, scope, firstResExpr, false)
	if err != nil {
		return err
	}

	elseIfs := n.Leaves[2]
	if elseIfs != nil {
		for _, elseif := range elseIfs.Leaves {
			cond := elseif.Leaves[0]
			err = resolveExpr(M, sy, scope, cond, false)
			if err != nil {
				return err
			}
			resExpr := elseif.Leaves[1]
			err = resolveExpr(M, sy, scope, resExpr, false)
			if err != nil {
				return err
			}
		}
	}
	_else := n.Leaves[3]
	if _else != nil {
		return resolveExpr(M, sy, scope, _else.Leaves[0], false)
	}
	return nil
}

func resolveSwitchExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	cond := n.Leaves[0]
	err := resolveExpr(M, sy, scope, cond, false)
	if err != nil {
		return err
	}
	valueCaseList := n.Leaves[2]
	if valueCaseList == nil {
		fmt.Println(sy.N)
		fmt.Println(n)
	}
	for _, vcase := range valueCaseList.Leaves {
		exps := vcase.Leaves[0]
		for _, expr := range exps.Leaves {
			err := resolveExpr(M, sy, scope, expr, false)
			if err != nil {
				return err
			}
		}

		resExpr := vcase.Leaves[1]
		err = resolveExpr(M, sy, scope, resExpr, false)
		if err != nil {
			return err
		}
	}
	def := n.Leaves[3]
	if def != nil {
		return resolveExpr(M, sy, scope, def.Leaves[0], false)
	}
	return nil
}

// -0---v v-------1-------------v v-2-----------------
// proc F [a:SomeSum] do match a as x case ... then ...
func resolveMatchExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	cond := n.Leaves[0]
	err := resolveExpr(M, sy, scope, cond, false)
	if err != nil {
		return err
	}
	as := n.Leaves[1]
	if as != nil {
		newScope := &ir.Scope{
			Parent:  scope,
			Symbols: map[string]*ir.Symbol{},
		}
		sy := &ir.Symbol{
			Kind:     sk.Local,
			Name:     as.Lexeme.Text,
			N:        as,
			M:        M,
			External: false,
		}
		newScope.Add(as.Lexeme.Text, sy)
		scope = newScope
		as.Scope = newScope
	}
	typeCaseList := n.Leaves[2]
	for _, tcase := range typeCaseList.Leaves {
		texps := tcase.Leaves[0]
		err := resolveTypeExprs(M, sy, scope, false, false, texps.Leaves...)
		if err != nil {
			return err
		}

		resExpr := tcase.Leaves[1]
		err = resolveExpr(M, sy, scope, resExpr, false)
		if err != nil {
			return err
		}
	}
	def := n.Leaves[3]
	if def != nil {
		return resolveExpr(M, sy, scope, def.Leaves[0], false)
	}
	return nil
}

func resolveBlockExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	newScope := &ir.Scope{
		Parent:  scope,
		Symbols: map[string]*ir.Symbol{},
	}
	n.Scope = newScope
	for _, leaf := range n.Leaves {
		err := resolveExpr(M, sy, newScope, leaf, false)
		if err != nil {
			return err
		}
	}
	return nil
}

func resolveArrayMapLitExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	annot := n.Leaves[0]
	fields := n.Leaves[1]

	err := resolveTypeExpr(M, sy, scope, false, false, annot)
	if err != nil {
		return err
	}
	for _, leaf := range fields.Leaves {
		if leaf.Lexeme != nil && leaf.Lexeme.Kind == lk.Assign {
			keyExpr := leaf.Leaves[0]
			err := resolveExpr(M, sy, scope, keyExpr, isConst)
			if err != nil {
				return err
			}
			valExpr := leaf.Leaves[1]
			err = resolveExpr(M, sy, scope, valExpr, isConst)
			if err != nil {
				return err
			}
		} else {
			expr := leaf
			err := resolveExpr(M, sy, scope, expr, isConst)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resolveProductLitExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node, isConst bool) *Error {
	annot := n.Leaves[0]
	fields := n.Leaves[1]

	err := resolveTypeExpr(M, sy, scope, false, false, annot)
	if err != nil {
		return err
	}
	for _, leaf := range fields.Leaves {
		if leaf.Lexeme != nil && leaf.Lexeme.Kind == lk.Assign {
			field := leaf.Leaves[0]
			// validation of field names is deferred to typechecking
			if field.Lexeme.Kind != lk.Ident {
				return errorInvalidField(M, field)
			}
			expr := leaf.Leaves[1]
			err := resolveExpr(M, sy, scope, expr, isConst)
			if err != nil {
				return err
			}
		} else {
			expr := leaf
			err := resolveExpr(M, sy, scope, expr, isConst)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func resolveCallOrIndexExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, n *ir.Node) *Error {
	for _, leaf := range n.Leaves {
		var expr *ir.Node
		if leaf.Lexeme != nil && leaf.Lexeme.Kind == lk.Assign {
			field := leaf.Leaves[0]
			// validation of field names is deferred to typechecking
			if field.Lexeme.Kind != lk.Ident {
				return errorInvalidField(M, field)
			}
			// consider only right side
			expr = leaf.Leaves[1]
		} else {
			expr = leaf
		}
		// isConst is always false here, calls are not allowed
		// in global code
		err := resolveExpr(M, sy, scope, expr, false)
		if err != nil {
			return err
		}
	}
	return nil
}

// resolveTypeExpr makes a graph of global types
// so that we can check for badly recursive types
//
// good recursion happens when an edge can be ignored
// we can ignore recursion in case of maps, slices and procedures
// but only on *new* types, not on aliases or unamed types
//
// so we use two variables:
//     - ignore, to say we have a layer of indirection already
//     - allowRecursion, to say whether we're in a *new* type or not
func resolveTypeExpr(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, ignore, allowRecursion bool, n *ir.Node) *Error {
	if n == nil {
		return nil
	}
	var err *Error
	switch n.Kind {
	case nk.Sum, nk.Product:
		err = resolveTypeExprs(M, sy, scope, ignore, allowRecursion, n.Leaves...)
	case nk.Terminal:
		switch n.Lexeme.Kind {
		case lk.Arrow:
			key := n.Leaves[0]
			value := n.Leaves[1]
			err = resolveTypeExprs(M, sy, scope, true, allowRecursion, key, value)
		case lk.Star, lk.Ampersand:
			baseType := n.Leaves[0]
			err = resolveTypeExpr(M, sy, scope, true, allowRecursion, baseType)
		case lk.Proc:
			targs := n.Leaves[0]
			err = resolveTypeExprs(M, sy, scope, true, allowRecursion, targs.Leaves...)
			if err != nil {
				return err
			}
			tret := n.Leaves[1]
			err = resolveTypeExpr(M, sy, scope, true, allowRecursion, tret)
		case lk.QuestionMark:
			baseType := n.Leaves[0]
			err = resolveTypeExpr(M, sy, scope, ignore, allowRecursion, baseType)
		case lk.Dot:
			t := n.Leaves[1]
			err = resolveTypeExpr(M, sy, scope, ignore, allowRecursion, t)
		case lk.DoubleColon:
			// modules can't be recursive
			// so their symbols can't be either
		case lk.Ident:
			name := n.Lexeme.Text
			otherSy := M.Global.Find(name)
			if otherSy == nil {
				return errorSymbolNotDeclared(M, n)
			}
			if !otherSy.Kind.IsType() {
				return errorExpectedType(M, n)
			}
			if ignore {
				// we can only really ignore if recursion is allowed
				// otherwise we're either in an alias or unnamed type
				if allowRecursion {
					sy.IgnorableLink(otherSy)
				} else {
					sy.Link(otherSy)
				}
			} else {
				sy.Link(otherSy)
			}
		}
	}
	return err
}

func resolveTypeExprs(M *ir.Module, sy *ir.Symbol, scope *ir.Scope, ignore, allowRecursion bool, n ...*ir.Node) *Error {
	for _, node := range n {
		err := resolveTypeExpr(M, sy, scope, ignore, allowRecursion, node)
		if err != nil {
			return err
		}
	}
	return nil
}

func createImportedSymbols(M *ir.Module) *Error {
	header := M.Root.Leaves[0]
	if header.Kind != nk.Header {
		panic("bad node")
	}
	for _, n := range header.Leaves {
		switch n.Lexeme.Kind {
		case lk.Import:
			err := importSymbols(M, n)
			if err != nil {
				return err
			}
		case lk.From:
			err := fromImportSymbols(M, n)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func createGlobals(M *ir.Module) *Error {
	symbols := M.Root.Leaves[1]
	for _, leaf := range symbols.Leaves {
		switch leaf.Lexeme.Kind {
		case lk.Type:
			err := declareType(M, leaf)
			if err != nil {
				return err
			}
		default:
			sy := getSymbol(M, leaf)
			err := declareSymbol(M, sy)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkExports(M *ir.Module) *Error {
	// check if there are duplicated exports
	// look at global symbols and check if they exist
	// insert into the export list
	coupling := M.Root.Leaves[0]
	exported := map[string]*ir.Node{}
	for _, exp := range coupling.Leaves {
		if exp.Lexeme.Kind == lk.Export {
			for _, name := range exp.Leaves {
				_, ok := exported[name.Lexeme.Text]
				if ok {
					return errorDuplicatedExport(M, name)
				}
				exported[name.Lexeme.Text] = name
			}
		}
	}
	for name, n := range exported {
		oldsy := M.Global.Find(name)
		if oldsy == nil {
			return errorExportingUndefName(M, n)
		}
		M.Exported[name] = oldsy
	}
	return nil
}

func declareType(M *ir.Module, n *ir.Node) *Error {
	def := n.Leaves[1]
	sy := getSymbol(M, n)
	err := declareSymbol(M, sy)
	if err != nil {
		return err
	}
	if def.Lexeme.Kind == lk.Enum {
		return declareEnum(M, def)
	}
	return nil
}

func declareEnum(M *ir.Module, def *ir.Node) *Error {
	for _, leaf := range def.Leaves {
		var nameNode *ir.Node
		if leaf.Lexeme.Kind == lk.Is {
			nameNode = leaf.Leaves[0]
		} else {
			nameNode = leaf
		}
		sy := &ir.Symbol{
			Kind:     sk.TypeEnumOption,
			Name:     nameNode.Lexeme.Text,
			N:        leaf,
			M:        M,
			Refs:     map[string]*ir.Reference{},
			External: false,
		}
		err := declareSymbol(M, sy)
		if err != nil {
			return err
		}
	}
	return nil
}

func declareSymbol(M *ir.Module, sy *ir.Symbol) *Error {
	oldsy := M.Global.Find(sy.Name)
	if oldsy != nil {
		return errorNameAlreadyDefined(M, sy.N)
	}
	M.Global.Add(sy.Name, sy)
	return nil
}

func getSymbol(M *ir.Module, n *ir.Node) *ir.Symbol {
	name := getSymbolName(n)
	kind := getSymbolKind(n)
	return &ir.Symbol{
		Kind: kind,
		Name: name,
		N:    n,
		M:    M,
		Refs: map[string]*ir.Reference{},
	}
}

func getSymbolName(n *ir.Node) string {
	return n.Leaves[0].Lexeme.Text
}

func getSymbolKind(n *ir.Node) sk.SymbolKind {
	switch n.Lexeme.Kind {
	case lk.Type:
		def := n.Leaves[1]
		switch def.Lexeme.Kind {
		case lk.Is:
			return sk.TypeCreation
		case lk.As:
			return sk.TypeAlias
		case lk.Enum:
			return sk.TypeEnum
		}
	case lk.Proc:
		return sk.Procedure
	case lk.Const:
		return sk.Constant
	}
	panic("getSymbolType: what")
}

func importSymbols(M *ir.Module, n *ir.Node) *Error {
	for _, m := range n.Leaves {
		err := defineModSymbol(M, m)
		if err != nil {
			return err
		}
	}
	return nil
}

func defineModSymbol(M *ir.Module, n *ir.Node) *Error {
	var name string
	if n.Lexeme.Kind == lk.As {
		name = n.Leaves[1].Lexeme.Text
	} else {
		name = n.Lexeme.Text
	}
	sy := &ir.Symbol{
		Name: name,
		Kind: sk.Module,
		N:    n,
	}
	oldsy := M.Global.Find(name)
	if oldsy != nil {
		return errorNameAlreadyDefined(M, n)
	}
	M.Global.Add(name, sy)
	return nil
}

func fromImportSymbols(M *ir.Module, n *ir.Node) *Error {
	mod := n.Leaves[0].Lexeme.Text
	dep, ok := M.Dependencies[mod]
	if !ok {
		panic("dependency should have been found")
	}
	ids := n.Leaves[1]
	for _, leaf := range ids.Leaves {
		var name string
		var syNode *ir.Node
		if leaf.Lexeme.Kind == lk.As {
			name = leaf.Leaves[1].Lexeme.Text
			syNode = leaf.Leaves[0]
		} else {
			name = leaf.Lexeme.Text
			syNode = leaf
		}
		sy, ok := dep.M.Exported[syNode.Lexeme.Text]
		if !ok {
			return nameNotExported(M, syNode)
		}
		err := defineExternalSymbol(M, leaf, name, sy)
		if err != nil {
			return err
		}
	}
	return nil
}

func defineExternalSymbol(M *ir.Module, n *ir.Node, syName string, sy *ir.Symbol) *Error {
	newSy := &ir.Symbol{
		Name:     sy.Name,
		Kind:     sy.Kind,
		N:        sy.N,
		M:        sy.M,
		External: true,
	}
	oldsy := M.Global.Find(syName)
	if oldsy != nil {
		return errorNameAlreadyDefined(M, n)
	}
	M.Global.Add(syName, newSy)
	return nil
}

func resolveModule(s *state, modID string) (*ir.Module, *Error) {
	mod, ok := s.Modules[modID]
	if ok {
		return mod, nil
	}
	fileName, err := findFile(s, modID)
	if err != nil {
		return nil, err
	}
	n, err := openAndParse(s, fileName)
	if err != nil {
		return nil, err
	}
	mod = newModule(s.BaseFolder, modID, fileName, n)
	s.Modules[modID] = mod

	err = resolveDependencies(s, n.Leaves[0], mod)
	if err != nil {
		return nil, err
	}
	return mod, nil
}

func resolveDependencies(s *state, coupling *ir.Node, mod *ir.Module) *Error {
	if coupling.Kind != nk.Header {
		panic("resolution: resolveDependencies: bad node")
	}
	s.RefModule = mod
	for _, n := range coupling.Leaves {
		switch n.Lexeme.Kind {
		case lk.Import:
			err := multiImport(s, n, mod)
			if err != nil {
				return err
			}
		case lk.From:
			err := fromImport(s, n, mod)
			if err != nil {
				return err
			}
		case lk.Export:
			// must run after the symbol resolution
		}
	}
	return nil
}

func multiImport(s *state, n *ir.Node, dependentMod *ir.Module) *Error {
	if n.Lexeme.Kind != lk.Import {
		panic("resolver: singleImport: bad node")
	}

	for _, leaf := range n.Leaves {
		var alias string
		var modNode *ir.Node
		if leaf.Lexeme.Kind == lk.As {
			modNode = leaf.Leaves[0]
			alias = leaf.Leaves[1].Lexeme.Text
		} else {
			alias = leaf.Lexeme.Text
			modNode = leaf
		}

		s.RefNode = modNode

		mod, err := resolveModule(s, modNode.Lexeme.Text)
		if err != nil {
			err.Location = ir.Place(dependentMod, modNode)
			return err
		}

		addDependency(dependentMod, alias, mod, modNode)

		if err != nil {
			return err
		}
	}
	return nil
}

func fromImport(s *state, n *ir.Node, dependentMod *ir.Module) *Error {
	modNode := n.Leaves[0]
	modID := modNode.Lexeme.Text
	s.RefNode = n.Leaves[0]

	mod, err := resolveModule(s, modID)
	if err != nil {
		err.Location = ir.Place(dependentMod, modNode)
		return err
	}
	addDependency(dependentMod, modID, mod, n)
	return nil
}

func addDependency(parent *ir.Module, depName string, dependency *ir.Module, source *ir.Node) {
	_, ok := parent.Dependencies[depName]
	if ok {
		panic("resolver: addDependency: name already exists in dependent module")
	}
	parent.Dependencies[depName] = &ir.Dependency{M: dependency, Source: source}
}

type state struct {
	Modules       map[string]*ir.Module
	BaseFolder    string
	FilesInFolder []string

	RefNode   *ir.Node // for errors
	RefModule *ir.Module
}

func newState(fullPath string) (*state, error) {
	folder := getFolder(fullPath)
	files, err := ioutil.ReadDir(folder)
	if err != nil {
		return nil, err
	}
	filenames := make([]string, len(files))
	for i, file := range files {
		filenames[i] = file.Name()
	}
	return &state{
		Modules:       map[string]*ir.Module{},
		BaseFolder:    folder,
		FilesInFolder: filenames,
	}, nil
}
func extractName(filePath string) (string, *Error) {
	path := strings.Split(filePath, "/")
	name := strings.Split(path[len(path)-1], ".")
	if lexer.IsValidIdentifier(name[0]) {
		return name[0], nil
	}
	return "", &Error{
		Code:     et.InvalidFileName,
		Severity: sv.Error,
		Message:  filePath + " : " + name[0],
	}
}

func findFile(s *state, modID string) (string, *Error) {
	found := []string{}
	for _, filename := range s.FilesInFolder {
		if strings.HasPrefix(filename, modID+".") {
			found = append(found, filename)
		}
	}
	if len(found) > 1 {
		return "", ambiguousFilesInFolder(s.RefModule, s.RefNode, found, modID)
	}
	if len(found) == 0 {
		return "", moduleNotFound(s.RefModule, s.RefNode, s.BaseFolder, modID)
	}
	return found[0], nil
}

func openAndParse(s *state, filename string) (*ir.Node, *Error) {
	path := s.BaseFolder + "/" + filename
	text, e := ioutil.ReadFile(path)
	if e != nil {
		return nil, ProcessFileError(e)
	}

	n, err := parser.Parse(path, string(text))
	if err != nil {
		return nil, err
	}
	return n, nil
}

func getFolder(fullpath string) string {
	path := strings.Split(fullpath, "/")
	if len(path) == 1 {
		return ""
	}
	folder := strings.Join(path[:len(path)-1], "/")
	return folder
}

func invalidModuleName(filePath string) *Error {
	return &Error{
		Code:     et.InvalidFileName,
		Severity: sv.Error,
		Message:  filePath,
	}
}

func newModule(basePath, modID, filename string, root *ir.Node) *ir.Module {
	return &ir.Module{
		BasePath:     basePath,
		Name:         modID,
		FullPath:     basePath + "/" + filename,
		Root:         root,
		Dependencies: map[string]*ir.Dependency{},
		Exported:     map[string]*ir.Symbol{},
		Global: &ir.Scope{
			Parent:  ir.Universe,
			Symbols: map[string]*ir.Symbol{},
		},
		Visited: false,
	}
}

func checkDependencyCycles(M *ir.Module) *Error {
	for _, dep := range M.Dependencies {
		prev := []*ir.Dependency{}
		err := checkDepCycle(M, dep, prev)
		if err != nil {
			return err
		}
	}
	return nil
}

func checkDepCycle(M *ir.Module, d *ir.Dependency, prev []*ir.Dependency) *Error {
	if depHasVisited(prev, d) {
		return errorInvalidDependencyCycle(M, prev, d)
	}
	prev = append(prev, d)
	top := len(prev)
	for _, dep := range d.M.Dependencies {
		err := checkDepCycle(M, dep, prev[0:top])
		if err != nil {
			return err
		}
	}
	return nil
}

func depHasVisited(visited []*ir.Dependency, b *ir.Dependency) bool {
	for _, v := range visited {
		if b.M == v.M {
			return true
		}
	}
	return false
}

func checkAllTypeCycles(M *ir.Module) *Error {
	for _, sy := range M.Global.Symbols {
		if sy.External {
			continue
		}
		switch sy.Kind {
		case sk.TypeAlias, sk.TypeCreation, sk.TypeEnum:
			err := checkTypeCycle(M, sy, []*ir.Symbol{})
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkValCycles(M *ir.Module) *Error {
	for _, s := range M.Global.Symbols {
		if s.External {
			continue
		}
		if s.Kind == sk.Constant {
			prev := []*ir.Symbol{}
			err := checkConstCycle(M, s, prev)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkConstCycle(M *ir.Module, s *ir.Symbol, prev []*ir.Symbol) *Error {
	if hasVisited(prev, s) {
		prev = append(prev, s)
		return errorInvalidConstCycle(M, prev, s)
	}
	prev = append(prev, s)
	top := len(prev)
	for _, ref := range s.Refs {
		if ref.S.Kind == sk.Constant {
			err := checkConstCycle(M, ref.S, prev[0:top])
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func checkTypeCycle(M *ir.Module, sy *ir.Symbol, visited []*ir.Symbol) *Error {
	if hasVisited(visited, sy) {
		visited = append(visited, sy)
		return errorInvalidTypeCycle(M, sy, visited)
	}
	visited = append(visited, sy)
	top := len(visited)
	for _, ref := range sy.Refs {
		if !ref.Ignore {
			err := checkTypeCycle(M, ref.S, visited[0:top])
			if err != nil {
				return err
			}
		}
	}
	return nil
}
func hasVisited(visited []*ir.Symbol, b *ir.Symbol) bool {
	for _, v := range visited {
		if b == v {
			return true
		}
	}
	return false
}

func errorNameAlreadyDefined(M *ir.Module, newName *ir.Node) *Error {
	return ir.NewError(M, et.NameAlreadyDefined, newName, "name already exists")
}

func errorDuplicatedExport(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.DuplicatedExport, n, "named already exported")
}

func errorExportingUndefName(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.ExportingUndefName, n, "name not defined in this module")
}

func nameNotExported(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.NameNotExported, n, "name not defined or exported in module")
}

func errorInvalidDependencyCycle(M *ir.Module, prev []*ir.Dependency, dep *ir.Dependency) *Error {
	msg := dep.M.Name + ": forms a invalid cycle"
	for _, item := range prev {
		msg += item.M.Name + ": is imported by ^\n"
	}
	return ir.NewError(M, et.InvalidDependencyCycle, dep.Source, msg)
}

func ambiguousFilesInFolder(M *ir.Module, n *ir.Node, found []string, modID string) *Error {
	msg := "Multiple modules possible for " + modID +
		": " + strings.Join(found, ", ")
	if M != nil && n != nil {
		return ir.NewError(M, et.AmbiguousModuleName, n, msg)
	}
	return &Error{
		Code:    et.AmbiguousModuleName,
		Message: msg,
	}
}

func moduleNotFound(M *ir.Module, n *ir.Node, baseFolder string, modID string) *Error {
	msg := "module " + modID + " not found in folder " + baseFolder
	if M != nil && n != nil {
		return ir.NewError(M, et.ModuleNotFound, n, msg)
	}
	return &Error{
		Code:    et.ModuleNotFound,
		Message: msg,
	}
}

func errorInvalidConstCycle(M *ir.Module, visited []*ir.Symbol, s *ir.Symbol) *Error {
	msg := "'" + s.Name + "' forms a invalid cycle: "
	refs := []string{}
	for _, item := range visited {
		refs = append(refs, item.Name)
	}
	msg += strings.Join(refs, " <- ") + "\n"
	return ir.NewError(M, et.InvalidConstCycle, s.N, msg)
}

func errorInvalidTypeCycle(M *ir.Module, sy *ir.Symbol, visited []*ir.Symbol) *Error {
	msg := sy.Name + ": forms a invalid cycle: "
	refs := []string{}
	for _, item := range visited {
		refs = append(refs, item.Name)
	}
	msg += strings.Join(refs, " <- ") + "\n"
	return ir.NewError(M, et.InvalidTypeCycle, sy.N, msg)
}

func errorSymbolNotDeclared(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.SymbolNotDeclared, n, "symbol '"+n.Lexeme.Text+"' is not declared")
}

func errorExpectedType(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.ExpectedType, n, "symbol '"+n.Lexeme.Text+"' is not a type")
}

func errorInvalidField(M *ir.Module, n *ir.Node) *Error {
	return ir.NewError(M, et.InvalidField, n, "field must be an identifier")
}

func errorInvalidConstExpr(M *ir.Module, n *ir.Node, thing string) *Error {
	return ir.NewError(M, et.InvalidConstExpr, n, thing+" are not constant expressions")
}
