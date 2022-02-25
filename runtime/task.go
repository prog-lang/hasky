package runtime

import (
	"log"

	"github.com/sharpvik/hasky/runtime/opcode"
)

// Task represents a closure that encapsulates its environment and arguments for
// future execution.
//
// Task implements Object, Callable.
type Task struct {
	*Stack

	argc int
	argi int
	args []Object

	addr int
	env  *Environment
}

// NewMainTask returns a Task that expects no arguments - it represents the
// entrypoint of the whole program with the following type: Task ().
func NewMainTask(env *Environment) *Task {
	const entrypointInstructionAddress = 0
	const mainArgumentCount = 0
	return NewTask(mainArgumentCount, entrypointInstructionAddress, env)
}

func NewTask(argc, addr int, env *Environment) *Task {
	return &Task{
		Stack: NewStack(),

		argc: argc,
		argi: 0,
		args: make([]Object, argc),

		addr: addr,
		env:  env,
	}
}

func (t *Task) Type() Type {
	return TypeClosure
}

func (t *Task) Apply(arg Object) {
	t.args[t.argi] = arg
	t.argi++
}

func (t *Task) Call() (o Object) {
	for {
		if done := t.step(); done {
			break
		}
	}
	o = t.SafePop()
	log.Println("returning:", o)
	return
}

func (t *Task) step() (done bool) {
	instruction := t.fetch()
	log.Println("fetched instruction:", instruction)

	execute, done := t.decode(instruction)
	if done {
		return
	}
	log.Print("decoded instruction")

	execute(t)
	log.Print("executed instruction")
	return
}

func (t *Task) fetch() (instruction Instruction) {
	instruction = t.env.Code[t.addr]
	t.addr++
	return
}

func (t *Task) decode(instruction Instruction) (action Action, done bool) {
	if instruction.Opcode == opcode.Return {
		return nil, true
	}
	return Commands[instruction.Opcode](instruction.Operand), false
}
