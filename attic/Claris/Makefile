all: main.out

main.out : main.cpp
	g++ -Wall -O3 main.cpp -o main.out 

main.cpp : Main
	./Main
	indent -kr -i2 main.hpp
	indent -kr -i2 main.cpp

Main : Main.hs
	ghc -O3 --make -i../../ Main.hs

