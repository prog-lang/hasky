package runtime

import (
	"bufio"
	"io"
	"os"

	"github.com/prog-lang/hasky/machine"
)

func RunFile(name string) (err error) {
	file, err := os.Open(name)
	if err != nil {
		return
	}
	defer file.Close()
	return RunReader(file)
}

func RunReader(rd io.Reader) (err error) {
	r, err := initReader(rd)
	if err != nil {
		return
	}

	data, err := machine.ReadData(r)
	if err != nil {
		return
	}

	code, err := machine.ReadCode(r)
	if err != nil {
		return
	}

	machine.Start(machine.NewEnv(data, code, Core))
	return
}

func initReader(r io.Reader) (bufr *bufio.Reader, err error) {
	bufr = bufio.NewReader(r)
	_, err = bufr.ReadBytes('\n')
	return
}
