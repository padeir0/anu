package parser

import (
	. "abc/core"
	ek "abc/core/errorkind"
	lk "abc/core/lexeme/lexkind"
	nk "abc/core/module/nodekind"
	sv "abc/core/severity"

	lex "abc/core/lexeme"
	mod "abc/core/module"

	lxr "abc/lexer"

	"fmt"
)

func Parse(filename string, contents string) (*mod.Node, *Error) {
	l := lxr.NewLexer(filename, contents)
	err := l.Next()
	if err != nil {
		return nil, err
	}
	n, err := module(l)
	if err != nil {
		return nil, err
	}
	computeRanges(n)
	return n, nil
}

// Module = Header Body
func module(l *lxr.Lexer) (*mod.Node, *Error) {
	head, err := header(l)
	if err != nil {
		return nil, err
	}
	bod, err := body(l)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: []*mod.Node{head, bod},
		Kind:   nk.Module,
		Range:  nil,
	}, nil
}

// Header = (ModuleRelations ";"?)*
func header(l *lxr.Lexer) (*mod.Node, *Error) {
	relations, err := repeat(l, moduleRelationsSemicolon)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Kind:   nk.Header,
		Leaves: relations,
		Range:  nil,
	}, nil
}

// ModuleRelations ";"?
func moduleRelationsSemicolon(l *lxr.Lexer) (*mod.Node, *Error) {
	modrel, err := moduleRelation(l)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.Semicolon {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
	}
	return modrel, nil
}

// ModuleRelations = Import | Export | FromImport
func moduleRelation(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Import:
		return _import(l)
	case lk.Export:
		return _export(l)
	case lk.From:
		return _fromImport(l)
	}
	return nil, nil
}

// Import = "import" AliasList
func _import(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Import)
	if err != nil {
		return nil, err
	}
	list, err := aliasList(l)
	if err != nil {
		return nil, err
	}
	kw.Leaves = list
	return kw, nil
}

// FromImport = "from" id "import" AliasList
func _fromImport(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.From)
	if err != nil {
		return nil, err
	}
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}

	_, err = expect(l, lk.Import)
	if err != nil {
		return nil, err
	}

	list, err := aliasList(l)
	if err != nil {
		return nil, err
	}
	listNode := &mod.Node{Kind: nk.IdList}

	listNode.Leaves = list
	kw.Leaves = []*mod.Node{id, listNode}

	return kw, nil
}

// Export = "export" ("all" | AliasList)
func _export(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Export)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.All {
		n, err := consume(l)
		if err != nil {
			return nil, err
		}
		kw.Leaves = []*mod.Node{n}
		return kw, nil
	}
	list, err := aliasList(l)
	if err != nil {
		return nil, err
	}
	kw.Leaves = list
	return kw, nil
}

// AliasList = Alias ("," Alias)* ","?
func aliasList(l *lxr.Lexer) ([]*mod.Node, *Error) {
	list, err := repeatCommaList(l, alias)
	if err != nil {
		return nil, err
	}
	if list == nil {
		return nil, newCompilerError(l, ek.ExpectedProd, "expected at least one alias")
	}
	return list, nil
}

// Alias = id ("as" id)?
func alias(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind == lk.Ident {
		id, err := consume(l)
		if err != nil {
			return nil, err
		}
		if l.Word.Kind == lk.As {
			as, err := consume(l)
			if err != nil {
				return nil, err
			}
			secId, err := expect(l, lk.Ident)
			if err != nil {
				return nil, err
			}
			as.Leaves = []*mod.Node{id, secId}
			return as, nil
		}
		return id, nil
	}
	return nil, nil
}

// Body = (Symbol ";"?)*
func body(l *lxr.Lexer) (*mod.Node, *Error) {
	symbols, err := repeat(l, symbolSemicolon)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Kind:   nk.Header,
		Leaves: symbols,
		Range:  nil,
	}, nil
}

// Symbol ";"?
func symbolSemicolon(l *lxr.Lexer) (*mod.Node, *Error) {
	symbol, err := symbol(l)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.Semicolon {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
	}
	return symbol, nil
}

// Symbol = Constant | Procedure | Type
func symbol(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Const:
		return _constDef(l)
	case lk.Proc:
		return _procDef(l)
	case lk.Type:
		return _typeDef(l)
	}
	return nil, nil
}

// Constant = "const" id TypeAnnot? "=" Expr
func _constDef(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Const)
	if err != nil {
		return nil, err
	}
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	var annot *mod.Node
	if l.Word.Kind == lk.Colon {
		annot, err = typeAnnot(l)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(l, lk.Assign)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}

	kw.Leaves = []*mod.Node{id, annot, exp}
	return kw, nil
}

// Procedure = "proc" id TypeAnnot? Signature "do" Expr
func _procDef(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Proc)
	if err != nil {
		return nil, err
	}
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	var annot *mod.Node
	if l.Word.Kind == lk.Colon {
		annot, err = typeAnnot(l)
		if err != nil {
			return nil, err
		}
	}

	sig, err := signature(l)
	if err != nil {
		return nil, err
	}

	_, err = expect(l, lk.Do)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}

	kw.Leaves = []*mod.Node{id, annot, sig, exp}
	return kw, nil
}

// Expr = And ("or" And)*
func expr(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, and, "expression", isOr)
}

// And = Comp ("and" Comp)*
func and(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, comp, "expression", isAnd)
}

// Comp = Sum (compOp Sum)*
func comp(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, sum, "expression", isCompOp)
}

// Sum = Mult (sumOp Mult)*
func sum(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, mult, "expression", isSumOp)
}

// Mult = UnaryPrefix (multOp UnaryPrefix)*
func mult(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, unaryPrefix, "expression", isMultOp)
}

// UnaryPrefix = Prefix* UnarySuffix
func unaryPrefix(s *lxr.Lexer) (*mod.Node, *Error) {
	preFirst, preLast, err := repeatUnaryLeft(s, prefix)
	if err != nil {
		return nil, err
	}

	suff, err := unarySuffix(s)
	if err != nil {
		return nil, err
	}
	if preFirst != nil && suff == nil {
		msg := "expected expression after prefix operator"
		err := newCompilerError(s, ek.ExpectedProd, msg)
		return nil, err
	}
	if suff == nil {
		return nil, nil
	}

	if preFirst != nil {
		preLast.AddLeaf(suff)
		suff = preFirst
	}

	return suff, nil
}

// Prefix = "&" | "@" | "not" | "~" | "$"
func prefix(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Ampersand, lk.At, lk.Not, lk.Tilde, lk.Dollar:
		return consume(l)
	}
	return nil, nil
}

// UnarySuffix = Factor Suffix*
func unarySuffix(s *lxr.Lexer) (*mod.Node, *Error) {
	fact, err := factor(s)
	if err != nil {
		return nil, err
	}
	suFirst, suLast, err := repeatUnaryRight(s, suffix)
	if err != nil {
		return nil, err
	}
	if suFirst != nil {
		suFirst.AddLeaf(fact)
		fact = suLast
	}
	return fact, nil
}

// Suffix = PropertyAccess | CallOrIndex | TypeAnnot | TypeReturn
func suffix(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Dot:
		return propertyAccess(l)
	case lk.LeftBracket:
		return callOrIndex(l)
	case lk.Colon:
		return typeAnnot(l)
	case lk.Caret:
		return typeReturn(l)
	}
	return nil, nil
}

// PropertyAccess = "." id
func propertyAccess(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Dot {
		return nil, nil
	}
	dot, err := consume(l)
	if err != nil {
		return nil, err
	}
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	dot.AddLeaf(id)
	return dot, nil
}

// TypeReturn = "^" TypeExpr
func typeReturn(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Caret {
		return nil, nil
	}
	caret, err := consume(l)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	caret.AddLeaf(t)
	return caret, nil
}

// Signature = (Arguments (Return)?)?
func signature(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.LeftBracket {
		return nil, nil
	}
	args, err := arguments(l)
	if err != nil {
		return nil, err
	}
	var ret *mod.Node
	if l.Word.Kind != lk.Do {
		ret, err = typeExpr(l)
		if err != nil {
			return nil, err
		}
	}
	return &mod.Node{
		Leaves: []*mod.Node{args, ret},
		Kind:   nk.Signature,
	}, nil
}

// CallOrIndex = "[" FieldList "]"
func callOrIndex(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.LeftBracket {
		return nil, nil
	}
	_, err := expect(l, lk.LeftBracket)
	if err != nil {
		return nil, err
	}
	var fields []*mod.Node
	if l.Word.Kind != lk.RightBracket {
		fields, err = repeatCommaList(l, field)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(l, lk.RightBracket)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: fields,
		Kind:   nk.CallOrIndex,
	}, nil
}

// FieldList = Field ("," Field)* ","?
// Field = Expr ("=" Expr)?
func field(l *lxr.Lexer) (*mod.Node, *Error) {
	exp, err := expr(l)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.Assign {
		eq, err := consume(l)
		if err != nil {
			return nil, err
		}
		exp2, err := expectProd(l, expr, "expression")
		if err != nil {
			return nil, err
		}
		eq.Leaves = []*mod.Node{exp, exp2}
		return eq, nil
	}
	return exp, nil
}

/*
Factor
= Name | int | bool | char | string | nil
| ArrayMapLit | ProductLit | For | Switch
| If | NestedExpr | Block | EarlyReturn | Set | Let | New
*/
func factor(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Ident:
		return name(l)
	case lk.IntLit, lk.I8Lit, lk.I16Lit, lk.I32Lit, lk.I64Lit,
		lk.True, lk.False, lk.StringLit, lk.Nil, lk.CharLit:
		return consume(l)
	case lk.Slash:
		return arrayMapLit(l)
	case lk.LeftBrace:
		return productLit(l)
	case lk.For:
		return _for(l)
	case lk.Switch:
		return _switch(l)
	case lk.If:
		return _if(l)
	case lk.LeftParen:
		return nestedExpr(l)
	case lk.Begin:
		return block(l)
	case lk.Return:
		return earlyReturn(l)
	case lk.Set:
		return _set(l)
	case lk.Let:
		return _let(l)
	case lk.New:
		return _new(l)
	}
	return nil, nil
}

// If = "if" Expr "then" Expr ElseIf* Else?
func _if(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.If)
	if err != nil {
		return nil, err
	}
	cond, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.Then)
	if err != nil {
		return nil, err
	}
	firstRes, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	elseifs, err := repeat(l, elseif)
	if err != nil {
		return nil, err
	}
	elseiflist := &mod.Node{
		Leaves: elseifs,
		Kind:   nk.ElseIfList,
	}
	_else, err := _else(l)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{cond, firstRes, elseiflist, _else}
	return kw, nil
}

// ElseIf = "elseif" Expr "then" Expr
func elseif(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Elseif {
		return nil, nil
	}
	kw, err := consume(l)
	if err != nil {
		return nil, err
	}
	cond, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.Then)
	if err != nil {
		return nil, err
	}
	resExpr, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{cond, resExpr}
	return kw, nil
}

// Else = "else" Expr
func _else(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Else)
	if err != nil {
		return nil, err
	}
	result, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(result)
	return kw, nil
}

// For = "for" (Conditional | Iterative | Range)
func _for(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.For)
	if err != nil {
		return nil, err
	}
	var body *mod.Node
	switch l.Word.Kind {
	case lk.Each:
		body, err = iterative(l)
		if err != nil {
			return nil, err
		}
	case lk.Range:
		body, err = ranged(l)
		if err != nil {
			return nil, err
		}
	default:
		body, err = conditional(l)
		if err != nil {
			return nil, err
		}
	}
	kw.AddLeaf(body)
	return kw, nil
}

// Iterative = "each" id ("," id)? "in" Expr "do" Expr
func iterative(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.Each)
	if err != nil {
		return nil, err
	}
	firstId, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	var secId *mod.Node
	if l.Word.Kind == lk.Comma {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
		secId, err = expect(l, lk.Ident)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(l, lk.In)
	if err != nil {
		return nil, err
	}
	collection, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.Do)
	if err != nil {
		return nil, err
	}
	res, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: []*mod.Node{firstId, secId, collection, res},
		Kind:   nk.IterativeFor,
	}, nil
}

// Conditional = Expr "do" Expr
func conditional(l *lxr.Lexer) (*mod.Node, *Error) {
	cond, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.Do)
	if err != nil {
		return nil, err
	}
	res, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: []*mod.Node{cond, res},
		Kind:   nk.ConditionalFor,
	}, nil
}

// Range = "range" Expr "to" Expr ("as" id)? "do" Expr
func ranged(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.Range)
	if err != nil {
		return nil, err
	}
	initial, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.To)
	if err != nil {
		return nil, err
	}
	target, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	var as *mod.Node
	if l.Word.Kind == lk.As {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
		as, err = expect(l, lk.Ident)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(l, lk.Do)
	if err != nil {
		return nil, err
	}
	res, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: []*mod.Node{initial, target, as, res},
		Kind:   nk.RangedFor,
	}, nil
}

// Switch = "switch" (TypeSwitch | ValueSwitch)
func _switch(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Switch)
	if err != nil {
		return nil, err
	}
	var body *mod.Node
	if l.Word.Kind == lk.Type {
		body, err = typeSwitch(l)
		if err != nil {
			return nil, err
		}
	} else {
		body, err = valueSwitch(l)
		if err != nil {
			return nil, err
		}
	}
	kw.AddLeaf(body)
	return kw, nil
}

func valueSwitch(l *lxr.Lexer) (*mod.Node, *Error) {
	cond, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	cases, err := repeat(l, valueCase)
	if err != nil {
		return nil, err
	}
	valueCaseList := &mod.Node{
		Leaves: cases,
		Kind:   nk.ValueCaseList,
	}
	var def *mod.Node
	if l.Word.Kind == lk.Default {
		def, err = _default(l)
		if err != nil {
			return nil, err
		}
	}

	return &mod.Node{
		Leaves: []*mod.Node{cond, valueCaseList, def},
		Kind:   nk.ValueSwitch,
	}, nil
}

// ValueCase = "case" ExprList "then" Expr
func valueCase(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Case {
		return nil, nil
	}
	kw, err := consume(l)
	if err != nil {
		return nil, err
	}
	exps, err := repeatCommaList(l, expr)
	if err != nil {
		return nil, err
	}
	expsList := &mod.Node{
		Leaves: exps,
		Kind:   nk.ExpressionList,
	}
	_, err = expect(l, lk.Then)
	if err != nil {
		return nil, err
	}
	result, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{expsList, result}
	return kw, nil
}

// TypeSwitch = "type" Expr ("as" id) TypeCase* Default?
func typeSwitch(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.Type)
	if err != nil {
		return nil, err
	}
	cond, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	var as *mod.Node
	if l.Word.Kind == lk.As {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
		as, err = expect(l, lk.Ident)
		if err != nil {
			return nil, err
		}
	}
	cases, err := repeat(l, typeCase)
	if err != nil {
		return nil, err
	}
	typeCaseList := &mod.Node{
		Leaves: cases,
		Kind:   nk.TypeCaseList,
	}
	var def *mod.Node
	if l.Word.Kind == lk.Default {
		def, err = _default(l)
		if err != nil {
			return nil, err
		}
	}
	return &mod.Node{
		Leaves: []*mod.Node{cond, as, typeCaseList, def},
		Kind:   nk.TypeSwitch,
	}, nil
}

// TypeCase = "case" TypeExprList "then" Expr
func typeCase(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Case {
		return nil, nil
	}
	kw, err := consume(l)
	if err != nil {
		return nil, err
	}
	texps, err := repeatCommaList(l, typeExpr)
	if err != nil {
		return nil, err
	}
	texpsList := &mod.Node{
		Leaves: texps,
		Kind:   nk.TypeExprList,
	}
	_, err = expect(l, lk.Then)
	if err != nil {
		return nil, err
	}
	result, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{texpsList, result}
	return kw, nil
}

// Default = "default" Expr
func _default(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Default)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(exp)
	return kw, nil
}

// Let = "let" LetDeclList ("in" Expr)?
func _let(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Let)
	if err != nil {
		return nil, err
	}
	decls, err := letDeclList(l)
	if err != nil {
		return nil, err
	}
	var inExp *mod.Node
	if l.Word.Kind == lk.In {
		_, err = consume(l)
		if err != nil {
			return nil, err
		}
		inExp, err = expectProd(l, expr, "expression")
		if err != nil {
			return nil, err
		}
	}
	kw.Leaves = []*mod.Node{decls, inExp}
	return kw, nil
}

// LetDeclList = LetDecl ("," LetDecl)* ","?
func letDeclList(l *lxr.Lexer) (*mod.Node, *Error) {
	decls, err := repeatCommaList(l, letDecl)
	if err != nil {
		return nil, err
	}
	if decls == nil {
		return nil, newCompilerError(l, ek.ExpectedProd, "let must have at least one declaration")
	}
	return &mod.Node{
		Leaves: decls,
		Kind:   nk.LetDeclList,
	}, nil
}

// LetDecl = VarList "=" Expr
func letDecl(l *lxr.Lexer) (*mod.Node, *Error) {
	vars, err := repeatCommaList(l, decl)
	if err != nil {
		return nil, err
	}
	if vars == nil {
		return nil, nil
	}
	assign, err := expect(l, lk.Assign)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	varlist := &mod.Node{
		Leaves: vars,
		Kind:   nk.VarList,
	}
	assign.Leaves = []*mod.Node{varlist, exp}
	return assign, nil
}

// Set = "set" ExprList assignOp Expr
func _set(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Set)
	if err != nil {
		return nil, err
	}
	exprs, err := repeatCommaList(l, expr)
	if err != nil {
		return nil, err
	}
	if exprs == nil {
		return nil, newCompilerError(l, ek.ExpectedProd, "expected at least one expression")
	}
	assignees := &mod.Node{
		Leaves: exprs,
		Kind:   nk.ExpressionList,
	}
	assignOp, err := expect(l, lk.Assign, lk.MinusAssign,
		lk.PlusAssign, lk.MultiplicationAssign,
		lk.ConcatAssign, lk.Swap, lk.Remove)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, expr, "expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{assignees, assignOp, exp}
	return kw, nil
}

// New = "new" TypeAnnot "[" FieldList? "]"
func _new(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.New)
	if err != nil {
		return nil, err
	}
	annot, err := typeAnnot(l)
	if err != nil {
		return nil, err
	}

	_, err = expect(l, lk.LeftBracket)
	if err != nil {
		return nil, err
	}
	var fields *mod.Node
	if l.Word.Kind != lk.RightBracket {
		fs, err := repeatCommaList(l, field)
		if err != nil {
			return nil, err
		}
		fields = &mod.Node{
			Leaves: fs,
			Kind:   nk.FieldList,
		}
	}
	_, err = expect(l, lk.RightBracket)
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{annot, fields}
	return kw, nil
}

// EarlyReturn = "return" Expr?
func earlyReturn(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Return)
	if err != nil {
		return nil, err
	}
	exp, err := expr(l)
	if err != nil {
		return nil, err
	}
	kw.AddLeaf(exp)
	return kw, nil
}

// NestedExpr = "(" Expr ")"
func nestedExpr(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftParen)
	if err != nil {
		return nil, err
	}
	exp, err := expr(l)
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.RightParen)
	if err != nil {
		return nil, err
	}
	return exp, nil
}

// ProductLit = ComplexLitBody
func productLit(l *lxr.Lexer) (*mod.Node, *Error) {
	n, err := complexLitBody(l)
	if err != nil {
		return nil, err
	}
	n.Kind = nk.ProductLit
	return n, nil
}

// ArrayMapLit = "\" ComplexLitBody
func arrayMapLit(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.Slash)
	if err != nil {
		return nil, err
	}
	n, err := complexLitBody(l)
	if err != nil {
		return nil, err
	}
	n.Kind = nk.ArrayMapLit
	return n, nil
}

// ComplexLitBody = "{" TypeAnnot? FieldList? "}"
func complexLitBody(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftBrace)
	if err != nil {
		return nil, err
	}
	var annot *mod.Node
	if l.Word.Kind == lk.Colon {
		annot, err = typeAnnot(l)
		if err != nil {
			return nil, err
		}
	}
	var fields *mod.Node
	if l.Word.Kind != lk.RightBrace {
		fs, err := repeatCommaList(l, field)
		if err != nil {
			return nil, err
		}
		fields = &mod.Node{
			Leaves: fs,
			Kind:   nk.FieldList,
		}
	}
	_, err = expect(l, lk.RightBrace)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: []*mod.Node{annot, fields},
		Kind:   nk.InvalidNodeKind, // this is set upwards
	}, nil
}

// Block = "begin" ExprSemicolon* "end"
func block(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.Begin)
	if err != nil {
		return nil, err
	}
	exprs, err := repeat(l, exprSemicolon)
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.End)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: exprs,
		Kind:   nk.Block,
	}, nil
}

// ExprSemicolon = Expr ";"?
func exprSemicolon(l *lxr.Lexer) (*mod.Node, *Error) {
	exp, err := expr(l)
	if err != nil {
		return nil, err
	}

	if l.Word.Kind == lk.Semicolon {
		_, err := consume(l)
		if err != nil {
			return nil, err
		}
	}
	return exp, nil
}

// Arguments = "[" DeclList? "]"
func arguments(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftBracket)
	if err != nil {
		return nil, err
	}
	var list []*mod.Node
	if l.Word.Kind != lk.RightBracket {
		list, err = repeatCommaList(l, decl)
	}
	_, err = expect(l, lk.RightBracket)
	if err != nil {
		return nil, err
	}
	if list == nil {
		return nil, nil
	}
	return &mod.Node{
		Leaves: list,
		Kind:   nk.Arguments,
	}, nil
}

// Decl = id TypeAnnot?
func decl(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Ident {
		return nil, nil
	}
	id, err := consume(l)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.Colon {
		ta, err := typeAnnot(l)
		if err != nil {
			return nil, err
		}
		// reorder to make sense
		ta.Leaves = []*mod.Node{id, ta.Leaves[0]}
		return ta, nil
	}
	return id, nil
}

// Type = "type" id (TypeAlias|TypeCreation|Enum)
func _typeDef(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Type)
	if err != nil {
		return nil, err
	}
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	err = check(l, lk.Is, lk.As, lk.Enum)
	if err != nil {
		return nil, err
	}
	var def *mod.Node
	switch l.Word.Kind {
	case lk.Is:
		def, err = typeCreation(l)
		if err != nil {
			return nil, err
		}
	case lk.As:
		def, err = typeAlias(l)
		if err != nil {
			return nil, err
		}
	case lk.Enum:
		def, err = enum(l)
		if err != nil {
			return nil, err
		}
	}
	kw.Leaves = []*mod.Node{id, def}
	return kw, nil
}

// TypeAlias = "as" TypeExpr
func typeAlias(l *lxr.Lexer) (*mod.Node, *Error) {
	as, err := expect(l, lk.As)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	as.AddLeaf(t)
	return as, nil
}

// TypeCreation = "is" TypeExpr
func typeCreation(l *lxr.Lexer) (*mod.Node, *Error) {
	is, err := expect(l, lk.Is)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	is.AddLeaf(t)
	return is, nil
}

// Enum = "enum" EnumOption ("," EnumOption)* ","?
func enum(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Enum)
	if err != nil {
		return nil, err
	}
	list, err := repeatCommaList(l, enumOption)
	if err != nil {
		return nil, err
	}
	if list == nil {
		return nil, newCompilerError(l, ek.ExpectedProd, "enums must have at least one option")
	}
	kw.Leaves = list
	return kw, nil
}

// EnumOption = id ("is" TypeExpr)?
func enumOption(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind != lk.Ident {
		return nil, nil // better error upwards
	}
	id, err := consume(l)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.Is {
		is, err := consume(l)
		if err != nil {
			return nil, err
		}

		t, err := expectProd(l, typeExpr, "type expression")
		if err != nil {
			return nil, err
		}
		is.Leaves = []*mod.Node{id, t}
		return is, nil
	}
	return id, nil
}

// TypeAnnot = ":" TypeExpr
func typeAnnot(l *lxr.Lexer) (*mod.Node, *Error) {
	colon, err := expect(l, lk.Colon)
	if err != nil {
		return nil, err
	}
	exp, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	colon.AddLeaf(exp)
	return colon, nil
}

// TypeExpr = Map ("|" Map)*
func typeExpr(l *lxr.Lexer) (*mod.Node, *Error) {
	sums, err := repeatList(l, _map, isPipe)
	if err != nil {
		return nil, err
	}
	if sums == nil {
		return nil, nil
	}
	if len(sums) == 1 {
		return sums[0], nil
	}
	texp := &mod.Node{
		Leaves: sums,
		Kind:   nk.Sum,
	}
	return texp, nil
}

// Map = TypeFactor ("->" TypeFactor)*
func _map(l *lxr.Lexer) (*mod.Node, *Error) {
	return repeatBinary(l, typeFactor, "type", isArrow)
}

// TypeFactor = TypePrefix* SingleType
func typeFactor(l *lxr.Lexer) (*mod.Node, *Error) {
	start, end, err := repeatUnaryLeft(l, typePrefix)
	if err != nil {
		return nil, err
	}
	stype, err := singleType(l)
	if err != nil {
		return nil, err
	}
	if start != nil && stype == nil {
		msg := "expected type after prefix type operator"
		err := newCompilerError(l, ek.ExpectedProd, msg)
		return nil, err
	}
	if stype == nil {
		return nil, nil
	}
	if start != nil {
		end.AddLeaf(stype)
		stype = start
	}
	return stype, nil
}

// TypePrefix = Ref | ArrayType | Optional
// ArrayType = "*"
// Optional = "?"
// Ref = "&"
func typePrefix(l *lxr.Lexer) (*mod.Node, *Error) {
	switch l.Word.Kind {
	case lk.Star, lk.QuestionMark, lk.Ampersand:
		return consume(l)
	}
	return nil, nil
}

// SingleType = Name | NestedType | ProcType | ProductType | nil
func singleType(l *lxr.Lexer) (*mod.Node, *Error) {
	err := check(l, lk.Ident, lk.LeftParen, lk.LeftBrace, lk.Proc, lk.Nil)
	if err != nil {
		return nil, err
	}
	switch l.Word.Kind {
	case lk.Ident:
		return name(l)
	case lk.LeftParen:
		return nestedType(l)
	case lk.LeftBrace:
		return productType(l)
	case lk.Proc:
		return procType(l)
	case lk.Nil:
		return consume(l)
	}
	// because of the repeatCommaList
	return nil, nil
}

// Name = id ("::" id)?
func name(l *lxr.Lexer) (*mod.Node, *Error) {
	id, err := expect(l, lk.Ident)
	if err != nil {
		return nil, err
	}
	if l.Word.Kind == lk.DoubleColon {
		dc, err := consume(l)
		if err != nil {
			return nil, err
		}
		secID, err := expect(l, lk.Ident)
		if err != nil {
			return nil, err
		}
		dc.Leaves = []*mod.Node{id, secID}
		return dc, nil
	}
	return id, nil
}

// NestedType = "(" TypeExpr ")"
func nestedType(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftParen)
	if err != nil {
		return nil, err
	}
	t, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	_, err = expect(l, lk.RightParen)
	if err != nil {
		return nil, err
	}
	return t, nil
}

// ProcType = "proc" TypeArguments TypeExpr
func procType(l *lxr.Lexer) (*mod.Node, *Error) {
	kw, err := expect(l, lk.Proc)
	if err != nil {
		return nil, err
	}
	targ, err := typeArguments(l)
	if err != nil {
		return nil, err
	}
	tret, err := expectProd(l, typeExpr, "type expression")
	if err != nil {
		return nil, err
	}
	kw.Leaves = []*mod.Node{targ, tret}
	return kw, nil
}

// TypeArguments = "[" TypeExprList? "]"
func typeArguments(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftBracket)
	if err != nil {
		return nil, err
	}
	args := []*mod.Node{}
	if l.Word.Kind != lk.RightBracket {
		args, err = repeatCommaList(l, typeExpr)
		if err != nil {
			return nil, err
		}
	}
	_, err = expect(l, lk.RightBracket)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: args,
		Kind:   nk.TypeArgs,
	}, nil
}

// ProductType = "{" Naming ("," Naming)* ","? "}"
func productType(l *lxr.Lexer) (*mod.Node, *Error) {
	_, err := expect(l, lk.LeftBrace)
	if err != nil {
		return nil, err
	}

	items, err := repeatCommaList(l, naming)
	if err != nil {
		return nil, err
	}

	if items == nil {
		return nil, newCompilerError(l, ek.ExpectedProd, "product types must have at least one field (otherwise, use nil)")
	}

	_, err = expect(l, lk.RightBrace)
	if err != nil {
		return nil, err
	}
	return &mod.Node{
		Leaves: items,
		Kind:   nk.Product,
	}, nil
}

// Naming = ("." id)? UnaryType
func naming(l *lxr.Lexer) (*mod.Node, *Error) {
	if l.Word.Kind == lk.Dot {
		dot, err := consume(l)
		if err != nil {
			return nil, err
		}
		id, err := expect(l, lk.Ident)
		if err != nil {
			return nil, err
		}
		t, err := expectProd(l, typeExpr, "type expression")
		if err != nil {
			return nil, err
		}
		dot.Leaves = []*mod.Node{id, t}
		return dot, nil
	}
	return typeExpr(l)
}

func isOr(l *lex.Lexeme) bool {
	return l.Kind == lk.Or
}
func isAnd(l *lex.Lexeme) bool {
	return l.Kind == lk.And
}

func isCompOp(l *lex.Lexeme) bool {
	switch l.Kind {
	case lk.Equals, lk.Different, lk.Greater,
		lk.Less, lk.GreaterOrEquals, lk.LessOrEquals, lk.Is:
		return true
	}
	return false
}

func isSumOp(l *lex.Lexeme) bool {
	switch l.Kind {
	case lk.Plus, lk.Minus, lk.DoubleDot:
		return true
	}
	return false
}

func isMultOp(l *lex.Lexeme) bool {
	switch l.Kind {
	case lk.Star, lk.Division, lk.Remainder:
		return true
	}
	return false
}

func isPipe(l *lex.Lexeme) bool {
	return l.Kind == lk.Pipe
}

func isArrow(l *lex.Lexeme) bool {
	return l.Kind == lk.Arrow
}

func consume(l *lxr.Lexer) (*mod.Node, *Error) {
	lexeme := l.Word
	err := l.Next()
	if err != nil {
		return nil, err
	}
	rng := lexeme.Range
	n := &mod.Node{
		Lexeme: lexeme,
		Leaves: []*mod.Node{},
		Kind:   nk.Terminal,
		Range:  &rng,
		T:      nil,
	}
	return n, nil
}

func check(l *lxr.Lexer, tpList ...lk.LexKind) *Error {
	for _, tp := range tpList {
		if l.Word.Kind == tp {
			return nil
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found '%v'",
		tpList,
		l.Word.Kind)

	err := newCompilerError(l, ek.ExpectedSymbol, message)
	return err
}

func expect(l *lxr.Lexer, tpList ...lk.LexKind) (*mod.Node, *Error) {
	for _, tp := range tpList {
		if l.Word.Kind == tp {
			return consume(l)
		}
	}
	message := fmt.Sprintf("Expected one of %v: instead found %v",
		tpList,
		l.Word.Kind)

	err := newCompilerError(l, ek.ExpectedSymbol, message)
	return nil, err
}

func expectProd(l *lxr.Lexer, prod production, name string) (*mod.Node, *Error) {
	n, err := prod(l)
	if err != nil {
		return nil, err
	}
	if n == nil {
		message := fmt.Sprintf("expected %v instead found %v", name, l.Word.Kind)
		err := newCompilerError(l, ek.ExpectedProd, message)
		return nil, err
	}
	return n, err
}

type production func(l *lxr.Lexer) (*mod.Node, *Error)
type validator func(*lex.Lexeme) bool

/* repeatBinary implements the following pattern
for a given Production and Terminal:

	repeatBinary := Production {Terminal Production}

Validator checks for terminals.
Left to Right precedence
*/
func repeatBinary(l *lxr.Lexer, prod production, name string, v validator) (*mod.Node, *Error) {
	last, err := prod(l)
	if err != nil {
		return nil, err
	}
	if last == nil {
		return nil, nil
	}
	for v(l.Word) {
		parent, err := consume(l)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(last)

		newLeaf, err := expectProd(l, prod, name)
		if err != nil {
			return nil, err
		}
		parent.AddLeaf(newLeaf)

		last = parent
	}
	return last, nil
}

/* repeat implements the following pattern
for a given Production:

	repeat := {Production}.
*/
func repeat(l *lxr.Lexer, prod production) ([]*mod.Node, *Error) {
	out := []*mod.Node{}
	n, err := prod(l)
	if err != nil {
		return nil, err
	}
	if n == nil {
		return nil, nil
	}
	for n != nil {
		out = append(out, n)
		n, err = prod(l)
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

/*repeatUnaryLeft implements the following pattern
for a given Production:

	repeatUnaryLeft := {Production}.

But returns the first and last item in the tree.

It's Left associative: first<-second<-last
*/
func repeatUnaryLeft(l *lxr.Lexer, prod production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(l)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(l)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		last.AddLeaf(n)
		last = n
	}
	return first, last, nil
}

func repeatUnaryRight(l *lxr.Lexer, prod production) (*mod.Node, *mod.Node, *Error) {
	first, err := prod(l)
	if err != nil {
		return nil, nil, err
	}
	if first == nil {
		return nil, nil, nil
	}
	last := first
	for first != nil {
		n, err := prod(l)
		if err != nil {
			return nil, nil, err
		}
		if n == nil {
			break
		}
		n.AddLeaf(last)
		last = n
	}
	return first, last, nil
}

/* repeatList implements the following pattern
for a given Production and Terminal:

	repeatList := Production {Terminal Production}

Validator checks for terminals.

It differs from RepeatBinary in that it returns a slice
instead of a Tree with precedence
*/
func repeatList(l *lxr.Lexer, prod production, val validator) ([]*mod.Node, *Error) {
	first, err := prod(l)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for val(l.Word) {
		l.Next()
		n, err := prod(l)
		if err != nil {
			return nil, err
		}
		out = append(out, n)
	}
	return out, nil
}

// Implements the pattern:
//    RepeatCommaList := Production {',' Production} [','].
func repeatCommaList(l *lxr.Lexer, prod production) ([]*mod.Node, *Error) {
	first, err := prod(l)
	if err != nil {
		return nil, err
	}
	if first == nil {
		return nil, nil
	}
	out := []*mod.Node{first}
	for l.Word.Kind == lk.Comma {
		l.Next()
		n, err := prod(l)
		if err != nil {
			return nil, err
		}
		if n != nil {
			out = append(out, n)
		}
	}
	if l.Word.Kind == lk.Comma {
		err := l.Next()
		if err != nil {
			return nil, err
		}
	}
	return out, nil
}

func expectedEOF(l *lxr.Lexer) *Error {
	return newCompilerError(l, ek.ExpectedEOF, "unexpected symbol, expected EOF")
}

func newCompilerError(l *lxr.Lexer, t ek.ErrorKind, message string) *Error {
	return &Error{
		Code:     t,
		Severity: sv.Error,
		Location: &Location{Range: &l.Word.Range, File: l.File},
		Message:  message,
	}
}

func computeRanges(curr *mod.Node) {
	if curr == nil {
		return
	}
	for _, leaf := range curr.Leaves {
		computeRanges(leaf)
	}
	for _, n := range curr.Leaves {
		if n == nil || n.Range == nil {
			continue
		}
		if curr.Range == nil {
			r := *n.Range
			curr.Range = &r
			continue
		}
		if curr.Range.Begin.MoreThan(n.Range.Begin) {
			curr.Range.Begin = n.Range.Begin
		}
		if curr.Range.End.LessThan(n.Range.End) {
			curr.Range.End = n.Range.End
		}
	}
}