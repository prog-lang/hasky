package assembler

import (
	"errors"
	"regexp"
	"strings"

	op "github.com/sharpvik/hasky/runtime/opcode"
)

var (
	ErrBadLineParse        = errors.New("bad line parse")
	ErrBadLabelParse       = errors.New("bad label parse")
	ErrBadInstructionParse = errors.New("bad isntruction parse")
)

type ErrorMap []ErrorLine

type ErrorLine struct {
	Number int
	Error  error
}

func parse(input string) (ast AST, errs ErrorMap) {
	lines := strings.Split(input, "\n")
	ast = make([]*ParsedLine, 0, len(lines))
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

func parseLine(line string) (parsed *ParsedLine, err error) {
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

func parseInstruction(line string) (opcode int, operand string, err error) {
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

func justOpcode(fields []string) (opcode int, operand string, err error) {
	opcodeString := fields[0]
	opcode, validOpcode := op.FromString(opcodeString)
	if !validOpcode {
		err = ErrBadInstructionParse
	}
	return
}

func opcodeAndOperand(fields []string) (opcode int, operand string, err error) {
	opcode, operand, err = justOpcode(fields)
	if err != nil {
		return
	}
	operand = fields[1]
	return
}
