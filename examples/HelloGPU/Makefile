all: main.out

main.out: main.cu dist/TableMaker.cu
	nvcc main.cu dist/TableMaker.cu -o main.out

dist/TableMaker.cu: Generator.hs
	runhaskell Generator.hs
