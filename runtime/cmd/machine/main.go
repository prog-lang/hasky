package main

import (
	"github.com/sharpvik/hasky/runtime"
	"github.com/sharpvik/hasky/runtime/bytecode"
)

func main() {
	m := runtime.NewMachine(nil,
		[]runtime.Instruction{
			{bytecode.Apply, 0},
			{bytecode.Apply, 0},
			{bytecode.Apply, 0},
			{bytecode.Apply, 0},
			{bytecode.Apply, 0},
			{bytecode.Apply, 0},
		})
	m.Start()
}
