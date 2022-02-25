package assembler

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sharpvik/hasky/runtime/bytecode"
)

func TestParseLabel(t *testing.T) {
	label, err := parseLabel("  main=  ")
	assert.NoError(t, err)
	assert.Equal(t, "main", label)

	label, err = parseLabel("  core:io:print=  ")
	assert.NoError(t, err)
	assert.Equal(t, "core:io:print", label)
}

func TestParseInstruction(t *testing.T) {
	opcode, operand, err := parseInstruction("  closure 0")
	assert.NoError(t, err)
	assert.Equal(t, bytecode.Closure, opcode)
	assert.Equal(t, 0, operand)

	_, _, err = parseInstruction("  task10 0")
	assert.ErrorIs(t, err, ErrBadInstructionParse)

	_, _, err = parseInstruction("  task9 hello")
	assert.ErrorIs(t, err, ErrBadInstructionParse)

	_, _, err = parseInstruction("  closure hello bye")
	assert.ErrorIs(t, err, ErrBadInstructionParse)
}
