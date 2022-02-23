package main

import (
	"io"
	"log"

	"github.com/sharpvik/hasky/runtime"
	"github.com/sharpvik/hasky/runtime/bytecode"
)

func init() {
	log.Default().SetOutput(io.Discard)
}

func main() {
	data := []runtime.Object{
		runtime.Int(2),
		runtime.Int(40),
	}

	code := []runtime.Instruction{
		{bytecode.Closure, 0},
		{bytecode.Closure, 1},
		{bytecode.Apply, 0},
		{bytecode.Apply, 1},
		{bytecode.Call, 0},
		{bytecode.Charge, 0},
		{bytecode.Call, 0},
		{bytecode.Return, 0},
	}

	runtime.Start(runtime.NewEnvironment(data, code))
}
