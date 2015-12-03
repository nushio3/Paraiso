all: main.out

main.out: main.cpp dist/Life.cpp
	g++ -Idist main.cpp dist/Life.cpp -o main.out

dist/Life.cpp: Generator.hs
	mkdir -p output
	stack runhaskell Generator.hs
