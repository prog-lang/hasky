package main

import (
	"bufio"
	"io"
	"log"
	"os"

	"github.com/sharpvik/hasky/runtime"
)

func main() {
	name := os.Args[1]
	file, err := os.Open(name)
	if err != nil {
		log.Fatal(err)
	}

	r, err := initReader(file)
	if err != nil {
		log.Fatal(err)
	}

	data, err := runtime.ReadData(r)
	if err != nil {
		log.Fatal(err)
	}

	code, err := runtime.ReadCode(r)
	if err != nil {
		log.Fatal(err)
	}

	runtime.Start(runtime.NewEnvironment(data, code))
}

func initReader(r io.Reader) (bufr *bufio.Reader, err error) {
	bufr = bufio.NewReader(r)
	_, err = bufr.ReadBytes('\n')
	return
}
