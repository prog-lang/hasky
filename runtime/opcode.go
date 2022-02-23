package runtime

var Opcode = []Mutator{
	closure(0),
	closure(1),
	closure(2),
	closure(3),
	closure(4),
	closure(5),
	closure(6),
	closure(7),
	closure(8),
	closure(9),
	native,
	apply,
	charge,
	call,
}

type Mutator func(*Machine, int)

func closure(argc int) func(*Machine, int) {
	return func(m *Machine, id int) {
		// m.Push(NewClosure(argc, m.Code[id]))
	}
}

func native(m *Machine, id int) {
	m.Push(m.std[id]())
}

func apply(m *Machine, id int) {
	m.Peek().(*Closure).Apply(m.Data[id])
}

func charge(m *Machine, _ int) {
	m.Peek().(*Closure).Apply(m.Pop())
}

func call(m *Machine, _ int) {
	m.Push(m.Pop().(*Closure).Call())
}
