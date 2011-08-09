all: hello.out

hello.out : hello.cpp
	g++ -O3 hello.cpp -o hello.out

hello.cpp : Main.hs 
	ghc --make -O3 Main.hs
	./Main | indent -kr -i2 > hello.cpp
