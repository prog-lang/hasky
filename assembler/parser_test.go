package assembler

import (
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"

	op "github.com/prog-lang/hasky/runtime/opcode"
)

func TestParseLines(t *testing.T) {
	const asm = `
	main:main=

		closure io:print
		push 1
		charge
		call
		return
	`
	ast, errs := ParseString(asm)
	assert.Len(t, errs, 0)
	assert.Len(t, ast, 6)
	label := ast[0]
	closure := ast[1]
	assert.Equal(t, NewLabel("main:main"), label)
	assert.Equal(t, NewInstruction(op.Closure, "io:print"), closure)
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
	opcode, operand, err := parseInstruction("closure core:print")
	assert.NoError(t, err)
	assert.Equal(t, op.Closure, opcode)
	assert.Equal(t, "core:print", operand)

	_, _, err = parseInstruction("task10 0")
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
	assert.Equal(t, "core:io", label.(*Label).Text)

	instruction, err := parseLine("  closure core:print")
	assert.NoError(t, err)
	assert.Equal(t,
		&Instruction{op.Closure, "core:print"},
		instruction.(*Instruction))
}

func ParseString(asm string) (AST, ErrorMap) {
	return Parse(LineStream(strings.NewReader(asm)))
}
