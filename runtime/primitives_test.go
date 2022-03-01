package runtime

import (
	"encoding/json"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestInt(t *testing.T) {
	i, err := json.Marshal(Int(42))
	assert.NoError(t, err)
	assert.Equal(t, []byte("42"), i)
}
