package assembler

import (
	"errors"
	"regexp"
	"strconv"
	"strings"
)

var (
	ErrBadLabelParse       = errors.New("bad label parse")
	ErrBadInstructionParse = errors.New("bad isntruction parse")
)

func parseLabel(line string) (label string, err error) {
	labelExpr := regexp.MustCompile(`^([\w:]+)=$`)
	matches := labelExpr.FindStringSubmatch(strings.TrimSpace(line))
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

	opcode, validOpcode := opcodes[opcodeString]
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
