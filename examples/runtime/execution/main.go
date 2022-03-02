package main

import (
	"io"
	"log"

	"github.com/sharpvik/hasky/runtime"
	"github.com/sharpvik/hasky/runtime/opcode"
)

func init() {
	log.Default().SetOutput(io.Discard)
}

func main() {
	data := runtime.Data{
		runtime.Int(2),
		runtime.Int(40),
	}

	code := []runtime.Instruction{
		{opcode.Closure, 0},
		{opcode.Closure, 1},
		{opcode.Apply, 0},
		{opcode.Apply, 1},
		{opcode.Call, 0},
		{opcode.Charge, 0},
		{opcode.Call, 0},
		{opcode.Return, 0},
	}

	runtime.Start(runtime.NewEnvironment(data, code))
}
