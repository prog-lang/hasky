package machine

type Evaluator func([]Object) Object

type LazyLambda func() *Lambda

// Lambda is a wrapper that encapsulates functional environment for native
// runtime operations (e.g. arithmetic operations).
//
// Lambda implements Object, Callable.
type Lambda struct {
	argc int
	argi int
	args []Object
	eval Evaluator
}

func NewLambda(argc int, eval Evaluator) *Lambda {
	return &Lambda{
		argc: argc,
		argi: 0,
		args: make([]Object, argc),
		eval: eval,
	}
}

func (c *Lambda) Object() {}

func (c *Lambda) Apply(arg Object) {
	c.args[c.argi] = arg
	c.argi++
}

func (c *Lambda) Call() Object {
	return c.eval(c.args)
}
