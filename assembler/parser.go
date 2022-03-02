package assembler

import (
	"errors"
	"regexp"
	"strconv"
	"strings"

	op "github.com/sharpvik/hasky/runtime/opcode"
)

const (
	TypeOperandInt = iota
	TypeOperandName
)

var (
	ErrBadLineParse        = errors.New("bad line parse")
	ErrBadLabelParse       = errors.New("bad label parse")
	ErrBadInstructionParse = errors.New("bad isntruction parse")
	ErrBadOperandParse     = errors.New("bad operand parse")
)

type ErrorMap []ErrorLine

type ErrorLine struct {
	Number int
	Error  error
}

func NewOperandName(name string) *TaggedUnion {
	return NewTaggedUnion(TypeOperandName, name)
}

func NewOperandInt(i int) *TaggedUnion {
	return NewTaggedUnion(TypeOperandInt, i)
}

func parse(input string) (ast AST, errs ErrorMap) {
	lines := strings.Split(input, "\n")
	ast = make([]*TaggedUnion, 0, len(lines))
	errs = make(ErrorMap, 0, len(lines))

	for i, line := range lines {
		parsed, err := parseLine(line)
		if err != nil {
			errs = append(errs, ErrorLine{i, err})
		} else if parsed != nil {
			ast = append(ast, parsed)
		}
	}

	return
}

func parseLine(line string) (parsed *TaggedUnion, err error) {
	trimmedLine := strings.TrimSpace(line)
	if len(trimmedLine) == 0 {
		return nil, nil
	}

	label, err := parseLabel(trimmedLine)
	if err == nil {
		return NewLabel(label), err
	}

	opcode, operand, err := parseInstruction(line)
	if err == nil {
		return NewInstruction(opcode, operand), err
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

func parseInstruction(line string) (opcode int32, operand string, err error) {
	fields := strings.Fields(line)
	switch len(fields) {
	case 1:
		return justOpcode(fields)

	case 2:
		return opcodeAndOperand(fields)

	default:
		err = ErrBadInstructionParse
		return
	}
}

func justOpcode(fields []string) (opcode int32, operand string, err error) {
	opcodeString := fields[0]
	opcode, validOpcode := op.FromString(opcodeString)
	if !validOpcode {
		err = ErrBadInstructionParse
	}
	return
}

func opcodeAndOperand(fields []string) (
	opcode int32,
	operand string,
	err error,
) {
	opcode, operand, err = justOpcode(fields)
	if err != nil {
		return
	}
	operand = fields[1]
	return
}

func parseOperand(operand string) (parsed *TaggedUnion, err error) {
	if operand == "" {
		return
	}
	if i, err := strconv.Atoi(operand); err == nil {
		return NewOperandInt(i), nil
	}
	nameExpr := regexp.MustCompile(`^([\w:]+)$`)
	if nameExpr.MatchString(operand) {
		return NewOperandName(operand), nil
	}
	return nil, ErrBadOperandParse
}
