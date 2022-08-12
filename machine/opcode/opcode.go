package opcode

import (
	"log"
)

const (
	/* Callables */
	CLOSURE int32 = iota
	TASK0
	TASK1
	TASK2
	TASK3
	TASK4
	TASK5
	TASK6
	TASK7
	TASK8
	TASK9
	APPLY
	CHARGE
	CALL

	/* Stack */
	PUSH

	RETURN
)

var opcodes = map[string]int32{
	"closure": CLOSURE,
	"task0":   TASK0,
	"task1":   TASK1,
	"task2":   TASK2,
	"task3":   TASK3,
	"task4":   TASK4,
	"task5":   TASK5,
	"task6":   TASK6,
	"task7":   TASK7,
	"task8":   TASK8,
	"task9":   TASK9,
	"apply":   APPLY,
	"charge":  CHARGE,
	"call":    CALL,
	"push":    PUSH,
	"return":  RETURN,
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
