package main

import (
	. "abc/core"
	"abc/pipelines"
	"abc/testing"

	"flag"
	"fmt"
	"os"
	"strconv"
	"strings"
)

var lexemes = flag.Bool("lexemes", false, "runs the lexer and prints the tokens")
var ast = flag.Bool("ast", false, "runs the lexer and parser, prints AST output")
var mod = flag.Bool("mod", false, "runs the resolution, prints Module output")

var test = flag.Bool("test", false, "runs tests for all files in a folder,")

var verbose = flag.Bool("v", false, "verbose tests")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) != 1 {
		Fatal("invalid number of arguments\n")
	}
	eval(args[0])
}

func eval(filename string) {
	checkValid()
	if *test {
		res := Test(filename)
		printResults(res)
		return
	}
	normalMode(filename)
}

func normalMode(filename string) {
	switch true {
	case *lexemes:
		lexemes, err := pipelines.Lexemes(filename)
		Check(err)
		output := []string{}
		for _, lexeme := range lexemes {
			output = append(output, lexeme.Text)
		}
		fmt.Println(strings.Join(output, ", "))
	case *ast:
		n, err := pipelines.Ast(filename)
		Check(err)
		fmt.Println(n)
	case *mod:
		m, err := pipelines.Mod(filename)
		Check(err)
		fmt.Println(m)
	default:
		_, err := pipelines.Compile(filename)
		Check(err)
	}
}

func checkValid() {
	var selected = []bool{*lexemes, *ast, *mod}
	var count = 0
	for _, b := range selected {
		if b {
			count++
		}
	}
	if count > 1 {
		Fatal("only one of lex, parse, hir, mir or asm flags may be used at a time")
	}
}

func Test(folder string) []*testing.TestResult {
	entries, err := os.ReadDir(folder)
	if err != nil {
		Fatal(err.Error() + "\n")
	}
	results := []*testing.TestResult{}
	for _, v := range entries {
		fullpath := folder + "/" + v.Name()
		if v.IsDir() {
			if *verbose {
				fmt.Print("\u001b[35m entering: " + fullpath + "\u001b[0m\n")
			}
			res := Test(fullpath)
			results = append(results, res...)
			if *verbose {
				fmt.Print("\u001b[35m leaving: " + fullpath + "\u001b[0m\n")
			}
		} else {
			res := testing.PartialTest(fullpath)
			results = append(results, &res)
			if *verbose {
				fmt.Print("testing: " + fullpath + "\t")
				fmt.Print(res.String() + "\n")
			}
		}
	}
	return results
}

func printResults(results []*testing.TestResult) {
	failed := 0
	fmt.Print("\n")
	for _, res := range results {
		if !res.Ok && res.Message != "" {
			fmt.Print(res.File + "\t" + res.Message + "\n")
		}
		if !res.Ok {
			failed += 1
		}
	}
	fmt.Print("\n")
	fmt.Print("failed: " + strconv.Itoa(failed) + "\n")
	fmt.Print("total: " + strconv.Itoa(len(results)) + "\n")
}

func Check(e *Error) {
	if e != nil {
		Fatal(e.String() + "\n")
	}
}

func Fatal(s string) {
	os.Stderr.Write([]byte(s))
	os.Exit(0)
}
