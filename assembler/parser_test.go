package assembler

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseLabel(t *testing.T) {
	label, consumed, err := parseLabel("  main:  ")
	assert.NoError(t, err)
	assert.True(t, consumed)
	assert.Equal(t, "main", label)

	label, consumed, err = parseLabel("  std.show:  ")
	assert.NoError(t, err)
	assert.True(t, consumed)
	assert.Equal(t, "std.show", label)
}
