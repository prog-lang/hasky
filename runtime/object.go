package runtime

import (
	"bufio"
	"bytes"
	"io"
	"strconv"
	"strings"
)

type Type int

const (
	TypeClosure Type = iota
	TypeUnit
	TypeInt
)

type Object interface {
	Type() Type
}

type Constant interface {
	Object
	Encode() []byte
}

type Data []Constant

type Callable interface {
	Object
	Apply(Object)
	Call() Object
}

func (data Data) Encode() (out []byte) {
	var buf bytes.Buffer
	for _, constant := range data {
		buf.Write(constant.Encode())
	}
	buf.WriteByte('\n')
	return buf.Bytes()
}

func ToConstant(raw []byte) (c Constant) {
	kind := raw[0]
	switch kind {
	case ConstTypeInt:
		i, err := strconv.Atoi(string(raw[1:]))
		if err != nil {
			panic(err)
		}
		c = Int(i)
	}
	return
}

func ReadConstants(r io.Reader) (data Data) {
	s := bufio.NewScanner(r)
	s.Split(bufio.ScanLines)
	for {
		if ok := s.Scan(); !ok {
			panic("failed to scan line")
		}
		text := strings.TrimSpace(s.Text())
		if text == "" {
			break
		}
		data = append(data, ToConstant([]byte(text)))
	}
	return
}
