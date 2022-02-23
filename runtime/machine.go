package runtime

import (
	"log"
	"os"
)

type Instruction struct {
	Opcode  uint8
	Operand int
}

type Thunk []Instruction

type Machine struct {
	Data []Object
	Code []Thunk

	std   []fn
	stack []Object
}

func NewMachine(data []Object, code []Thunk) *Machine {
	return &Machine{
		Data: data,
		Code: code,

		std:   std,
		stack: make([]Object, 0),
	}
}

func (m *Machine) Start() {
	defer m.Exit()
	for ; ; m.Step() {
	}
}

func (m *Machine) Step() {
	m.Execute(m.Code[0])
}

func (m *Machine) Execute(thunk Thunk) {
	for _, instruction := range thunk {
		m.Exec(instruction)
	}
}

func (m *Machine) Exec(instruction Instruction) {

}

func (m *Machine) Push(object Object) {
	m.stack = append(m.stack, object)
}

func (m *Machine) Pop() (object Object) {
	object = m.Peek()
	m.stack = m.stack[:len(m.stack)-1]
	return
}

func (m *Machine) Peek() Object {
	return m.stack[m.top()]
}

func (m *Machine) Exit() {
	if err := recover(); err != nil {
		log.Println("hasky exited with error:", err)
		os.Exit(1)
	}
}

func (m *Machine) top() int {
	return len(m.stack) - 1
}
