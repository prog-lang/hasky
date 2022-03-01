package runtime

import (
	"strconv"
)

const (
	ConstTypeInt byte = iota
)

var Nil Unit

type Unit struct{}

func (Unit) Type() Type {
	return TypeUnit
}

/* Constants */

type Int int

func (Int) Type() Type {
	return TypeInt
}

func (i Int) Encode() (out []byte, err error) {
	out = append([]byte{ConstTypeInt}, []byte(strconv.Itoa(int(i)))...)
	return append(out, '\n'), nil
}
