package assembler

import (
	"io"
	"os"
)

func AssembleFile(name string) (err error) {
	file, err := os.Open(name)
	if err != nil {
		return
	}

	input, err := io.ReadAll(file)
	if err != nil {
		return
	}

	ast, errs := Parse(string(input))
	if len(errs) != 0 {
		return errs
	}

	bc, err := ast.Bytecode()
	if err != nil {
		return
	}

	code, err := bc.Encode()
	if err != nil {
		return
	}

	out, err := createExecutableFile("out.ha.bin")
	if err != nil {
		return
	}

	_, err = out.Write(code)
	return
}

func createExecutableFile(name string) (out *os.File, err error) {
	return os.OpenFile(name, os.O_CREATE|os.O_EXCL|os.O_WRONLY, 0744)
}
