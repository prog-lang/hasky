package runtime

import (
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestConstants(t *testing.T) {
	data := Data{Int(2), Int(40)}

	encodedData, err := data.Encode()
	assert.NoError(t, err)

	decodedData, err := ReadConstants(bytes.NewBuffer(encodedData))
	assert.NoError(t, err)
	two := decodedData[0]
	forty := decodedData[1]
	assert.Equal(t, Int(2), two.(Int))
	assert.Equal(t, Int(40), forty.(Int))
}
