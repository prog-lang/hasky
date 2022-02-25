package assembler

import (
	"errors"
	"regexp"
	"strings"
)

var ErrBadLabelParse = errors.New("bad label parse")

func parseLabel(line string) (label string, consumed bool, err error) {
	labelExpr := regexp.MustCompile(`^([\w\.]+):$`)
	matches := labelExpr.FindStringSubmatch(strings.TrimSpace(line))
	if len(matches) == 0 {
		return "", false, ErrBadLabelParse
	}
	return matches[1], true, nil
}
