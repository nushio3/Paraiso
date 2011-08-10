all: simple.out vector.out thrust.out

simple.out : simple.cpp
	g++ -Wall -O3 simple.cpp -o simple.out 

simple.cpp : Simple
	./Simple

Simple : Simple.hs
	ghc -O3 --make -i../../ Simple.hs


vector.out : vector.cpp
	g++ -Wall -O3 vector.cpp -o vector.out 

vector.cpp : Vector
	./Vector

Vector : Vector.hs
	ghc -O3 --make -i../../ Vector.hs




thrust.out : thrust.cpp
	nvcc -O3 thrust.cu -o thrust.out 

thrust.cpp : Thrust
	./Thrust

Thrust : Thrust.hs
	ghc -O3 --make -i../../ Thrust.hs


