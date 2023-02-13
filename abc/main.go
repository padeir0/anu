package main

import (
	"abc/lexer"
	"abc/parser"
	"fmt"
)

func main() {
	l := lexer.NewLexer("test", "proc sum[a:int, b:int] int do a + b;")
	fmt.Println(lexer.AllTokens(l))

	root, err := parser.Parse("test", "proc sum[a:int, b:int] int do a + b;")
	if err != nil {
		fmt.Println("error", err)
		return
	}
	if root == nil {
		panic("root was nil")
	}
	fmt.Println(root.String())
}
