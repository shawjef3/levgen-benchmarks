.PHONY: go gccgo gcc clang clean

go: Go.go
	@echo ""
	go build -compiler=gc -o run-gcgo Go.go 
	time (./run-gcgo -v=1234 &> /dev/null)

gccgo: Go.go
	@echo ""
	go build -compiler=gccgo -gccgoflags="-O3" -o run-gccgo Go.go
	time (./run-gccgo -v=1234 &> /dev/null)

gcc: C.c
	@echo ""
	gcc -o run-gcc -O3 C.c
	time (./run-gcc 1234 &> /dev/null)

clang: C.c
	@echo ""
	clang -o run-clang -O3 C.c
	time (./run-clang 1234 &> /dev/null)

clean:
	rm -f run-gcgo run-gccgo run-gcc run-clang