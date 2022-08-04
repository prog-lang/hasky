package assembler

import (
	"bufio"
	"io"
	"strings"

	"github.com/getkalido/fungi"
)

type LineStreamer struct {
	*bufio.Reader
}

func LineStream(r io.Reader) fungi.Stream[string] {
	return &LineStreamer{bufio.NewReader(r)}
}

func (fstr *LineStreamer) Next() (line string, err error) {
	line, err = fstr.ReadString('\n')
	return strings.TrimSpace(line), err
}
