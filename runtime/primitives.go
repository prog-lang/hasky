package runtime

type Int int

func (Int) Type() Type {
	return TypeInt
}

type Unit struct{}

func (Unit) Type() Type {
	return TypeUnit
}

var Nil = Unit{}
