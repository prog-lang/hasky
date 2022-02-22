package runtime_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/sharpvik/hasky/runtime"
	"github.com/sharpvik/hasky/runtime/std"
)

func TestBasic(t *testing.T) {
	magic := std.NewClosure(1).
		Apply(runtime.Int(40)).
		Apply(runtime.Int(2)).
		Call()
	assert.Equal(t, 42, int(magic.(runtime.Int)))
}
