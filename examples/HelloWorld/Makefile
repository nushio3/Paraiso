all: main.out

main.out: main.cpp dist/TableMaker.cpp
	g++ main.cpp dist/TableMaker.cpp -o main.out

dist/TableMaker.cpp: Generator.hs
	runhaskell Generator.hs
