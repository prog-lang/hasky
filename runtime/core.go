package runtime

import (
	"fmt"
)

// Core is a list of Closure constructors for all native functions.
var Core = []ClosureConstructor{
	lazy(1, printout),
	lazy(2, add),
}

var CoreNames = map[string]int{
	"print": 0,
	"add":   1,
}

func ClosureAddressFromName(name string) (addr int32, exists bool) {
	address, exists := CoreNames[name]
	return int32(address), exists
}

func lazy(argc int, function Function) ClosureConstructor {
	return func() *Closure {
		return NewClosure(argc, function)
	}
}

func printout(args []Object) Object {
	fmt.Println(args[0])
	return Nil
}

func add(args []Object) Object {
	return Int(args[0].(Int) + args[1].(Int))
}
