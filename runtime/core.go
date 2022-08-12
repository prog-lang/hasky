package runtime

import (
	"fmt"

	"github.com/prog-lang/hasky/machine"
)

// Core is a list of lazy lambdas for all native functions.
var Core = []machine.LazyLambda{
	lazy(printout, 1),
	lazy(add, 2),
}

var CoreNames = map[string]int{
	"print": 0,
	"add":   1,
}

func LambdaAddr(name string) (addr int32, exists bool) {
	address, exists := CoreNames[name]
	return int32(address), exists
}

func lazy(fn machine.Evaluator, argc int) machine.LazyLambda {
	return func() *machine.Lambda {
		return machine.NewLambda(argc, fn)
	}
}

func printout(args []machine.Object) machine.Object {
	fmt.Println(args[0])
	return machine.Unit
}

func add(args []machine.Object) machine.Object {
	return machine.Int(args[0].(machine.Int) + args[1].(machine.Int))
}
