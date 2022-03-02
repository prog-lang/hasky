package main

import (
	"fmt"
	"io"
	"log"
	"os"
	"path"

	"github.com/sharpvik/hasky/assembler"
	"github.com/sharpvik/hasky/runtime"
)

func main() {
	disableLogging()
	err := run(os.Args)
	enableLogging()
	if err != nil {
		log.Fatal(err)
	}
}

func run(args []string) (err error) {
	name := args[1]
	switch path.Ext(name) {
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
