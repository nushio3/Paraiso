all: main.out

main.out: main.cpp dist/TableMaker.cu
	nvcc main.cpp dist/TableMaker.cu -o main.out

dist/TableMaker.cu: Generator.hs
	runhaskell Generator.hs
