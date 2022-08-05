package main

import (
	"io"
	"log"

	"github.com/prog-lang/hasky/machine"
	op "github.com/prog-lang/hasky/machine/opcode"
	"github.com/prog-lang/hasky/runtime"
)

func init() {
	log.Default().SetOutput(io.Discard)
}

func main() {
	data := machine.Data{
		machine.Int(2),
		machine.Int(40),
	}

	code := []machine.Instruction{
		{op.Closure, 0},
		{op.Closure, 1},
		{op.Apply, 0},
		{op.Apply, 1},
		{op.Call, 0},
		{op.Charge, 0},
		{op.Call, 0},
		{op.Return, 0},
	}

	machine.Start(machine.NewEnvironment(data, code, runtime.Core))
}
