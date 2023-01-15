package main

import (
	. "abc/core"
	"flag"
	"fmt"
	"os"
)

var printLexer = flag.Bool("tokens", false, "runs the lexer and prints the tokens")
var printParser = flag.Bool("ast", false, "runs the up to the parser, prints AST")
var printNameResolution = flag.Bool("names", false, "runs up to name resolution, prints Module")
var printTypeChecking = flag.Bool("types", false, "runs up to type checking, prints typed Module")
var printLinearization = flag.Bool("hir", false, "runs up to linearization, prints HIR")
var printUniqueChecking = flag.Bool("unique", false, "runs up to move checking, prints HIR with Frees")
var printLowering = flag.Bool("lir", false, "runs up to lowering, prints LIR")
var printAlloc = flag.Bool("mdir", false, "runs up to resalloc, prints MDIR")
var printAsm = flag.Bool("asm", false, "runs up to amd64 backend, prints fasm")

var test = flag.Bool("test", false, "runs tests for all files in a folder,"+
	" you can specify the stage to test using the other flags\n"+
	"\t ex: abc -tokens -test folder/\n"+
	"\t     abc -ast -test folder/")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) != 1 {
		Fatal("invalid number of arguments")
	}
	Check()
	if *test {
		Fatal("not implemented")
		return
	}
	Compile(args[0])
}

func Fatal(s string) {
	fmt.Println(s)
	os.Exit(1)
}

func FatalS(s fmt.Stringer) {
	fmt.Println(s)
	os.Exit(1)
}

func Compile(filename string) {
	if *printLexer || *printParser {
		lexParseOnly(filename)
	}
	furthest := FurthestPhase()

	module, errors := nameresolution.Resolve(filename)
	CheckErrors(errors)
	if *printNameResolution {
		fmt.Println(module)
	}
	if furthest == 2 {
		return
	}

	name := module.Name

	module, errors = typechecker.Check(module)
	CheckErrors(errors)
	if *printTypeChecking {
		fmt.Println(module)
	}
	if furthest == 3 {
		return
	}

	hir, errors := linearization.Linearize(module)
	CheckErrors(errors)
	if *printLinearization {
		fmt.Println(hir)
	}
	if furthest == 4 {
		return
	}

	hir, errors = uniquechecking.Check(hir)
	CheckErrors(errors)
	if *printUniqueChecking {
		fmt.Println(hir)
	}
	if furthest == 5 {
		return
	}

	lir, errors := lowering.Lower(hir)
	CheckErrors(errors)
	if *printLowering {
		fmt.Println(lir)
	}
	if furthest == 6 {
		return
	}

	mdir, errors := resalloc.Allocate(lir)
	CheckErrors(errors)
	if *printAlloc {
		fmt.Println(mdir)
	}
	if furthest == 7 {
		return
	}

	fasm := fasm.Fasmify(mdir)
	if *printAsm {
		fmt.Println(fasm)
	}
	if furthest == 8 {
		return
	}

	fasm.Compile(name, fasm)
}

func lexParseOnly(filename string) {
}

func FurthestPhase() int {
	phases := []bool{
		*printLexer,
		*printParser,
		*printNameResolution,
		*printTypeChecking,
		*printLinearization,
		*printUniqueChecking,
		*printLowering,
		*printAlloc,
		*printAsm,
	}
	// all phases by default (it's just a large number)
	furthest := 1 << 16
	for i, ok := range phases {
		if ok {
			furthest = i
		}
	}
	return furthest
}

func Check() {
	furthest := FurthestPhase()
	if (*printLexer || *printParser) && furthest > 1 {
		// this happens because the name resolution will also resolve modules,
		// and will need to call the parser and lexer recursively
		Fatal("running the lexer or parser can't be done with phases further than name resolution")
	}
}

func CheckErrors(diag Diagnostics) {
	if diag != nil {
		FatalS(diag)
	}
}
