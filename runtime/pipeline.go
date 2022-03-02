package runtime

import (
	"bufio"
	"io"
	"os"
)

func RunFile(name string) (err error) {
	file, err := os.Open(name)
	if err != nil {
		return
	}

	r, err := initReader(file)
	if err != nil {
		return
	}

	data, err := ReadData(r)
	if err != nil {
		return
	}

	code, err := ReadCode(r)
	if err != nil {
		return
	}

	Start(NewEnvironment(data, code))
	return
}

func initReader(r io.Reader) (bufr *bufio.Reader, err error) {
	bufr = bufio.NewReader(r)
	_, err = bufr.ReadBytes('\n')
	return
}
