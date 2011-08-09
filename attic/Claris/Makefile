all: simple.out vector.out

simple.out : simple.cpp
	g++ -Wall -O3 simple.cpp -o simple.out 

simple.cpp : Simple
	./Simple
	indent -kr -i2 simple.hpp
	indent -kr -i2 simple.cpp

Simple : Simple.hs
	ghc -O3 --make -i../../ Simple.hs


vector.out : vector.cpp
	g++ -Wall -O3 vector.cpp -o vector.out 

vector.cpp : Vector
	./Vector
	indent -kr -i2 vector.hpp
	indent -kr -i2 vector.cpp

Vector : Vector.hs
	ghc -O3 --make -i../../ Vector.hs


