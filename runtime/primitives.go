package runtime

import (
	"strconv"
)

const (
	ConstTypeInt byte = iota
)

type Int int

func (Int) Type() Type {
	return TypeInt
}

func (i Int) Encode() (out []byte) {
	out = append([]byte{ConstTypeInt}, []byte(strconv.Itoa(int(i)))...)
	return append(out, '\n')
}

type Unit struct{}

func (Unit) Type() Type {
	return TypeUnit
}

var Nil = Unit{}
