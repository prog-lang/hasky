package machine

import "fmt"

type Stack struct {
	stack []Object
}

func NewStack() *Stack {
	return new(Stack)
}

func (s *Stack) Push(o Object) {
	s.stack = append(s.stack, o)
}

func (s *Stack) Pop() (o Object) {
	top := s.top()
	o = s.stack[top]
	s.stack = s.stack[:top]
	return o
}

func (s *Stack) SafePop() (o Object) {
	if s.Empty() {
		return Unit
	}
	return s.Pop()
}

func (s *Stack) Peek() (o Object) {
	return s.stack[s.top()]
}

func (s *Stack) Len() int {
	return len(s.stack)
}

func (s *Stack) Empty() bool {
	return s.Len() == 0
}

func (s *Stack) String() string {
	return fmt.Sprint(s.stack)
}

func (s *Stack) top() int {
	return len(s.stack) - 1
}
