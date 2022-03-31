sim65:
	cl65 -t sim6502 hello.s; sim65 -v -c hello
build:
	cl65 -t none world.s
