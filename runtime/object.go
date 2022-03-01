package runtime

type Type int

const (
	TypeClosure Type = iota
	TypeUnit
	TypeInt
)

type Object interface {
	Type() Type
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
