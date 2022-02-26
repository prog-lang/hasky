package assembler

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sharpvik/hasky/runtime"
	"github.com/sharpvik/hasky/runtime/opcode"
	op "github.com/sharpvik/hasky/runtime/opcode"
)

func TestParseLines(t *testing.T) {
	const asm = `
	main:main=

		closure 0
		push 1
		charge 0
		call 0
		return 0
	`
	ast, errs := parse(asm)
	assert.Len(t, errs, 0)
	assert.Len(t, ast, 6)
	label := ast[0]
	closure := ast[1]
	assert.Equal(t, NewLabel("main:main"), label)
	assert.Equal(t, NewInstruction(opcode.Closure, 0), closure)
}

func TestParseLabel(t *testing.T) {
	label, err := parseLabel("main=")
	assert.NoError(t, err)
	assert.Equal(t, "main", label)

	label, err = parseLabel("core:io:print=")
	assert.NoError(t, err)
	assert.Equal(t, "core:io:print", label)
}

func TestParseInstruction(t *testing.T) {
	opcode, operand, err := parseInstruction("closure 0")
	assert.NoError(t, err)
	assert.Equal(t, op.Closure, opcode)
	assert.Equal(t, 0, operand)

	_, _, err = parseInstruction("task10 0")
	assert.ErrorIs(t, err, ErrBadInstructionParse)

	_, _, err = parseInstruction("task9 hello")
	assert.ErrorIs(t, err, ErrBadInstructionParse)

	_, _, err = parseInstruction("closure hello bye")
	assert.ErrorIs(t, err, ErrBadInstructionParse)
}

func TestParseLine(t *testing.T) {
	empty, err := parseLine("    \t")
	assert.NoError(t, err)
	assert.Nil(t, empty)

	label, err := parseLine("core:io=")
	assert.NoError(t, err)
	assert.Equal(t, "core:io", label.Value.(string))

	instruction, err := parseLine("  closure 1")
	assert.NoError(t, err)
	assert.Equal(t,
		runtime.Instruction{op.Closure, 1},
		instruction.Value.(runtime.Instruction))
}
