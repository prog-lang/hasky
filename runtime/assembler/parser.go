package assembler

import (
	"errors"
	"regexp"
	"strings"
)

var (
	ErrBadLineParse  = errors.New("bad line parse")
	ErrBadLabelParse = errors.New("bad label parse")
)

func parseLine(line string) (parsed interface{}, err error) {
	label, consumed, err := parseLabel(line)
	if err == nil {
		return label, nil
	}
	if consumed {
		return nil, err
	}
	return nil, ErrBadLineParse
}

func parseLabel(line string) (label string, consumed bool, err error) {
	labelExpr := regexp.MustCompile(`^([\w\.]+):$`)
	matches := labelExpr.FindStringSubmatch(strings.TrimSpace(line))
	if len(matches) == 0 {
		return "", false, ErrBadLabelParse
	}
	return matches[1], true, nil
}
