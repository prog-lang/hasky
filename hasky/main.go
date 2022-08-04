package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"log"
	"os"
	"os/exec"
	"path"

	"github.com/prog-lang/hasky/assembler"
	"github.com/prog-lang/hasky/runtime"
)

var ErrFailedToCompile = errors.New("failed to compile")

func main() {
	disableLogging()
	err := run(os.Args)
	enableLogging()
	if err != nil && err != ErrFailedToCompile {
		log.Fatal(err)
	}
}

func run(args []string) (err error) {
	name := args[1]
	switch path.Ext(name) {
	case ".ha":
		err = compileFile(name)

	case ".asm":
		err = assembler.AssembleFile(name)

	case ".bin":
		err = runtime.RunFile(name)

	default:
		err = fmt.Errorf("no idea what to do with this file: %s", name)
	}
	return
}

func disableLogging() {
	log.Default().SetOutput(io.Discard)
}

func enableLogging() {
	log.Default().SetOutput(os.Stdout)
}

func compileFile(name string) (err error) {
	buf, err := haskyCompile(name)
	if err != nil {
		return ErrFailedToCompile
	}
	return assembler.Assemble(buf)
}

func haskyCompile(name string) (r io.Reader, err error) {
	cmd := exec.Command("hasky-compile", name)
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = os.Stderr
	err = cmd.Run()
	r = &buf
	return
}
