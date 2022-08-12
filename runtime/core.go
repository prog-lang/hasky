package runtime

import (
	"fmt"

	"github.com/prog-lang/hasky/machine"
)

// Core is a list of Closure constructors for all native functions.
var Core = []machine.LambdaConstructor{
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

func lazy(argc int, function machine.Evaluator) machine.LambdaConstructor {
	return func() *machine.Lambda {
		return machine.NewClosure(argc, function)
	}
}

func printout(args []machine.Object) machine.Object {
	fmt.Println(args[0])
	return machine.Nil
}

func add(args []machine.Object) machine.Object {
	return machine.Int(args[0].(machine.Int) + args[1].(machine.Int))
}
