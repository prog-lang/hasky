package convert

func Int32AsBytes(i int32) (b []byte) {
	b = make([]byte, 4)
	for x := 0; x < 4; x++ {
		b[x] = byte(i >> (8 * x))
	}
	return
}

func BytesToInt32(b []byte) (i int32) {
	for x := 0; x < 4; x++ {
		i |= int32(b[x]) << (8 * x)
	}
	return
}
