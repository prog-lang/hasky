package std

import (
	"fmt"
	"github.com/sharpvik/hasky/runtime"
)

type libEntry struct {
	argc int
	fn   runtime.Function
}

var lib = []libEntry{
	{1, show},
	{2, add},
}

func NewClosure(id int) *runtime.Closure {
	entry := lib[id]
	return runtime.NewClosure(entry.argc, entry.fn)
}

func show(args []runtime.Object) runtime.Object {
	fmt.Println(args[0])
	return runtime.Unit{}
}

func add(args []runtime.Object) runtime.Object {
	return runtime.Int(args[0].(runtime.Int) + args[1].(runtime.Int))
}
