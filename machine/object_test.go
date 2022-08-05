package machine

import (
	"bufio"
	"bytes"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestConstants(t *testing.T) {
	data := Data{Int(2), Int(-40)}

	var buf bytes.Buffer
	err := data.EncodeAndWrite(&buf)
	assert.NoError(t, err)
	encodedData := buf.Bytes()

	decodedData, err := ReadData(bufio.NewReader(bytes.NewBuffer(encodedData)))
	assert.NoError(t, err)
	two := decodedData[0]
	forty := decodedData[1]
	assert.Equal(t, Int(2), two.(Int))
	assert.Equal(t, Int(-40), forty.(Int))
}
