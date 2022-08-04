package runtime

import (
	"bufio"
	"bytes"
	"errors"
	"io"

	"github.com/prog-lang/hasky/runtime/convert"
)

type Data []Constant

func (data Data) EncodeAndWrite(buf *bytes.Buffer) (err error) {
	for _, constant := range data {
		bytes, err := constant.Encode()
		if err != nil {
			return err
		}
		buf.Write(bytes)
	}
	buf.WriteByte(ConstListEnd)
	return
}

func ReadData(r *bufio.Reader) (data Data, err error) {
	for {
		constant, err := ReadConstant(r)
		if err != nil {
			return nil, err
		}
		if constant == nil {
			return data, nil
		}
		data = append(data, constant)
	}
}

func ReadConstant(r *bufio.Reader) (c Constant, err error) {
	kind, err := r.ReadByte()
	if err != nil {
		return
	}
	switch kind {
	case ConstTypeInt:
		return ReadConstantInt(r)

	case ConstListEnd:
		return nil, nil

	default:
		return nil, errors.New("unknown constant type")
	}
}

func ReadConstantInt(r *bufio.Reader) (decoded Int, err error) {
	raw := make([]byte, 4)
	if _, err = io.ReadFull(r, raw); err != nil {
		return
	}
	return Int(convert.BytesToInt32(raw)), nil
}
