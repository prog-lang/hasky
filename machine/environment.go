package machine

type Env struct {
	Data Data
	Code Code
	Core []LazyLambda
}

func NewEnv(
	data Data,
	code Code,
	core []LazyLambda,
) *Env {
	return &Env{
		Data: data,
		Code: code,
		Core: core,
	}
}
