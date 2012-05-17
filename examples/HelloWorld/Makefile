all: main.out

main.out: main.cpp dist/Hello.cpp
	g++ main.cpp dist/Hello.cpp -o main.out

dist/Hello.cpp: Hello.hs
	runhaskell Hello.hs
