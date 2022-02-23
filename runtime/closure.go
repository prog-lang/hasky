package runtime

type Function func([]Object) Object

type ClosureConstructor func() *Closure

// Closure is a wrapper that encapsulates functional environment for native
// runtime operations (e.g. arithmetic operations).
//
// Closure implements Object, Callable.
type Closure struct {
	argc int
	argi int
	args []Object
	eval Function
}

func NewClosure(argc int, eval Function) *Closure {
	return &Closure{
		argc: argc,
		argi: 0,
		args: make([]Object, argc),
		eval: eval,
	}
}

func (c *Closure) Type() Type {
	return TypeClosure
}

func (c *Closure) Apply(arg Object) {
	c.args[c.argi] = arg
	c.argi++
}

func (c *Closure) Call() Object {
	return c.eval(c.args)
}
