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
