package machine

import (
	"log"
	"os"
)

type Env struct {
	Data Data
	Code Code
	Core []LambdaConstructor
}

func NewEnv(
	data Data,
	code Code,
	core []LambdaConstructor,
) *Env {
	return &Env{
		Data: data,
		Code: code,
		Core: core,
	}
}

func Start(env *Env) {
	log.Print("starting runtime from environment ...")
	defer gracefulExit()
	setupAndStart(env)
}

func setupAndStart(env *Env) {
	log.Print("initialising the main thunk ...")
	main := MainTask(env)
	log.Print("starting execution by head call ...")
	main.Call()
}

func gracefulExit() {
	if err := recover(); err != nil {
		log.Println("hasky exited with error:", err)
		os.Exit(1)
	}
}
