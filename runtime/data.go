package runtime

import (
	"bufio"
	"bytes"
	"errors"
	"io"
	"strconv"
	"strings"
)

type Data []Constant

func (data Data) Encode() (out []byte, err error) {
	var buf bytes.Buffer
	for _, constant := range data {
		bytes, err := constant.Encode()
		if err != nil {
			return nil, err
		}
		buf.Write(bytes)
	}
	buf.WriteByte('\n')
	return buf.Bytes(), nil
}

func DecodeConstant(raw []byte) (c Constant, err error) {
	kind := raw[0]
	rawConstant := raw[1:]
	switch kind {
	case ConstTypeInt:
		return DecodeConstantInt(rawConstant)

	default:
		return nil, errors.New("unknown constant type")
	}
}

func DecodeConstantInt(raw []byte) (decoded Int, err error) {
	i, err := strconv.Atoi(string(raw))
	if err != nil {
		return
	}
	return Int(i), nil
}

func ReadConstants(r io.Reader) (data Data, err error) {
	s := bufio.NewScanner(r)
	s.Split(bufio.ScanLines)
	for {
		if ok := s.Scan(); !ok {
			return nil, errors.New("failed to scan line")
		}

		text := strings.TrimSpace(s.Text())
		if text == "" {
			break
		}

		constant, err := DecodeConstant([]byte(text))
		if err != nil {
			return nil, err
		}

		data = append(data, constant)
	}
	return
}
