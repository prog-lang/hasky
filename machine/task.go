package machine

import (
	"log"

	"github.com/prog-lang/hasky/machine/opcode"
)

// Task represents a function that encapsulates its environment and arguments
// for future execution. User defined functions are represented as Tasks under
// the hood.
//
// Task implements Object, Callable.
type Task struct {
	*Stack

	argc int
	argi int
	args []Object

	addr int32
	env  *Env
}

// MainTask returns a Task that expects no arguments - it represents the
// entrypoint of the whole program with the following type: Task ().
func MainTask(env *Env) *Task {
	const entrypointInstructionAddress = 0
	const mainArgumentCount = 0
	return NewTask(mainArgumentCount, entrypointInstructionAddress, env)
}

func NewTask(argc int, addr int32, env *Env) *Task {
	return &Task{
		Stack: NewStack(),

		argc: argc,
		argi: 0,
		args: make([]Object, argc),

		addr: addr,
		env:  env,
	}
}

func (t *Task) Object() {}

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
	log.Print("executed instruction:", t.Stack)
	return
}

func (t *Task) fetch() (instruction Instruction) {
	instruction = t.env.Code[t.addr]
	t.addr++
	return
}

func (t *Task) decode(instruction Instruction) (action Action, done bool) {
	if instruction.Opcode == opcode.RETURN {
		return nil, true
	}
	return InstructionSet[instruction.Opcode](instruction.Operand), false
}
