package runtime

type Function func([]Object) Object

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

func (c *Closure) Apply(arg Object) *Closure {
	c.args[c.argi] = arg
	c.argi++
	return c
}

func (c *Closure) Call() Object {
	return c.eval(c.args)
}
