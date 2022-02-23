package runtime

import (
	"log"
	"os"
)

type Instruction struct {
	Opcode  int
	Operand int
}

type Environment struct {
	Data []Object
	Code []Instruction
	Core []ClosureConstructor
}

func NewEnvironment(data []Object, code []Instruction) *Environment {
	return &Environment{
		Data: data,
		Code: code,
		Core: Core,
	}
}

func Start(env *Environment) {
	log.Print("starting runtime from environment ...")
	defer gracefulExit()
	setupAndStart(env)
}

func setupAndStart(env *Environment) {
	log.Print("initialising the main thunk ...")
	thunk := NewMainTask(env)
	log.Print("starting execution by head call ...")
	thunk.Call()
}

func gracefulExit() {
	if err := recover(); err != nil {
		log.Println("hasky exited with error:", err)
		os.Exit(1)
	}
}
