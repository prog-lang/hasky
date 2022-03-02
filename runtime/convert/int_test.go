package convert

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIntConvertions(t *testing.T) {
	var expected int32 = -42
	assert.Equal(t, expected, BytesToInt32(Int32AsBytes(expected)))
}
