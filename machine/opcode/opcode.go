package opcode

import (
	"log"
)

const (
	/* Callables */
	Closure int32 = iota
	Task0
	Task1
	Task2
	Task3
	Task4
	Task5
	Task6
	Task7
	Task8
	Task9
	Apply
	Charge
	Call

	/* Stack */
	Push

	Return
)

var opcodes = map[string]int32{
	"closure": Closure,
	"task0":   Task0,
	"task1":   Task1,
	"task2":   Task2,
	"task3":   Task3,
	"task4":   Task4,
	"task5":   Task5,
	"task6":   Task6,
	"task7":   Task7,
	"task8":   Task8,
	"task9":   Task9,
	"apply":   Apply,
	"charge":  Charge,
	"call":    Call,
	"push":    Push,
	"return":  Return,
}

func FromString(s string) (opcode int32, exists bool) {
	opcode, exists = opcodes[s]
	return
}

func MustFromString(s string) (opcode int32) {
	if opcode, exists := FromString(s); exists {
		return opcode
	}
	log.Fatalf("opcode %s does not exist", s)
	return
}
