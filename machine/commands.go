package machine

import "github.com/prog-lang/hasky/machine/opcode"

// InstructionSet array is a complete mapping of bytecode.Opcode to Command that
// it is meant to invoke.
var InstructionSet = [opcode.RETURN]Command{
	native,
	task(0),
	task(1),
	task(2),
	task(3),
	task(4),
	task(5),
	task(6),
	task(7),
	task(8),
	task(9),
	apply,
	charge,
	call,

	load,

	//! return opcode must not be included here
}

// Command is a functional representation of an opcode. Supply an operand to it
// and you'll get an Action.
type Command func(operand int32) Action

// Action is an effectful instruction that operates on a Task frame.
type Action func(*Task)

func native(operand int32) Action {
	return func(task *Task) {
		task.Push(task.env.Core[operand]())
	}
}

func task(argc int) Command {
	return func(operand int32) Action {
		return func(task *Task) {
			task.Push(NewTask(argc, operand, task.env))
		}
	}
}

func apply(operand int32) Action {
	return func(task *Task) {
		task.Peek().(Callable).Apply(task.env.Data[operand])
	}
}

func charge(operand int32) Action {
	return func(task *Task) {
		arg := task.Pop()
		task.Peek().(Callable).Apply(arg)
	}
}

func call(operand int32) Action {
	return func(task *Task) {
		task.Push(task.Pop().(Callable).Call())
	}
}

func load(operand int32) Action {
	return func(task *Task) {
		task.Push(task.env.Data[operand])
	}
}
