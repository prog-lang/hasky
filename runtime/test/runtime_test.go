package runtime_test

import (
	"testing"

	"github.com/prog-lang/hasky/machine"
	. "github.com/prog-lang/hasky/machine/opcode"
	"github.com/prog-lang/hasky/runtime"
	"github.com/stretchr/testify/assert"
)

func TestMachine(t *testing.T) {
	data := machine.Data{
		machine.Int(2),
		machine.Int(40),
	}

	code := []machine.Instruction{
		{CLOSURE, 0},
		{CLOSURE, 1},
		{APPLY, 0},
		{APPLY, 1},
		{CALL, 0},
		{CHARGE, 0},
		{CALL, 0},
		{RETURN, 0},
	}

	env := machine.NewEnv(data, code, runtime.Core)
	main := machine.MainTask(env)
	main.Call()
	assert.True(t, main.Stack.Empty())
}
