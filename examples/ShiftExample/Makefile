all: main.out

main.out: main.cpp dist/TableMaker.cpp
	g++ main.cpp -Idist-open dist-open/TableMaker.cpp -o main-open.out
	g++ main.cpp -Idist-cyclic dist-cyclic/TableMaker.cpp -o main-cyclic.out

dist/TableMaker.cpp: Generator.hs
	runhaskell Generator.hs
