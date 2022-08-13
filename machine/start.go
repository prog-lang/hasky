package machine

import (
	"log"
	"os"
)

func Start(env *Env) {
	defer gracefulExit()
	log.Print("starting execution by call to main ...")
	MainTask(env).Call()
}

func gracefulExit() {
	if err := recover(); err != nil {
		log.Println("hasky exited with error:", err)
		os.Exit(1)
	}
}
