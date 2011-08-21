all: simple.out vector.out thrust.out thrust_class.out 

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

thrust.out : thrust.cu
	nvcc -O3 thrust.cu -o thrust.out 

thrust.cu : Thrust
	./Thrust

Thrust : Thrust.hs
	ghc -O3 --make -i../../ Thrust.hs

thrust_class.out : thrust_class.cu
	nvcc -O3 thrust_class.cu -o thrust_class.out 

thrust_class.cu : ThrustClass
	./ThrustClass

ThrustClass : ThrustClass.hs
	ghc -O3 --make -i../../ ThrustClass.hs



