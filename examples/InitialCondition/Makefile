all: heart.png

heart.png:heart.txt
	gnuplot 'plot.gnu'

heart.txt:main.out
	./main.out

main.out: main.cpp dist/TableMaker.cpp
	g++ main.cpp dist/TableMaker.cpp -o main.out

dist/TableMaker.cpp: Generator.hs
	runhaskell Generator.hs
