package machine

type Object interface {
	Object()
}

type Constant interface {
	Object
	Encode() ([]byte, error)
}

type Callable interface {
	Object
	Apply(Object)
	Call() Object
}
