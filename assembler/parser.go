package assembler

import (
	"errors"
	"regexp"
	"strconv"
	"strings"

	"github.com/sharpvik/hasky/runtime"
	op "github.com/sharpvik/hasky/runtime/opcode"
)

var (
	ErrBadLineParse        = errors.New("bad line parse")
	ErrBadLabelParse       = errors.New("bad label parse")
	ErrBadInstructionParse = errors.New("bad isntruction parse")
)

func parseLine(line string) (parsed interface{}, err error) {
	trimmedLine := strings.TrimSpace(line)
	if len(trimmedLine) == 0 {
		return nil, nil
	}

	label, err := parseLabel(trimmedLine)
	if err == nil {
		return label, err
	}

	opcode, operand, err := parseInstruction(line)
	if err == nil {
		return runtime.Instruction{opcode, operand}, err
	}

	return nil, ErrBadLineParse
}

func parseLabel(line string) (label string, err error) {
	labelExpr := regexp.MustCompile(`^([\w:]+)=$`)
	matches := labelExpr.FindStringSubmatch(line)
	if len(matches) == 0 {
		return "", ErrBadLabelParse
	}
	return matches[1], nil
}

func parseInstruction(line string) (opcode int, operand int, err error) {
	fields := strings.Fields(line)
	if len(fields) != 2 {
		err = ErrBadInstructionParse
		return
	}

	opcodeString, operandString := fields[0], fields[1]

	opcode, validOpcode := op.FromString(opcodeString)
	if !validOpcode {
		err = ErrBadInstructionParse
		return
	}

	operand, err = strconv.Atoi(operandString)
	if err != nil {
		err = ErrBadInstructionParse
	}
	return
}
