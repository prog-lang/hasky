package runtime

var Opcode = [16]Command{
	closure,
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

	push,

	pass, // return does nothing - it's a mere unwind flag
}

type Command func(*Task, Instruction)

func closure(task *Task, instruction Instruction) {
	task.Push(task.env.Core[instruction.Operand]())
}

func task(argc int) Command {
	return func(task *Task, instruction Instruction) {
		task.Push(NewTask(argc, instruction.Operand, task.env))
	}
}

func apply(task *Task, instruction Instruction) {
	task.Peek().(Callable).Apply(task.env.Data[instruction.Operand])
}

func charge(task *Task, instruction Instruction) {
	arg := task.Pop()
	task.Peek().(Callable).Apply(arg)
}

func call(task *Task, instruction Instruction) {
	task.Push(task.Pop().(Callable).Call())
}

func push(task *Task, instruction Instruction) {
	task.Push(task.env.Data[instruction.Operand])
}

func pass(*Task, Instruction) {}
