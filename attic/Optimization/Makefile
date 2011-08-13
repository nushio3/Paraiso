all: diffusionMain.out

diffusionMain.out : diffusionMain.cpp
	g++ -Wall -O3 diffusionMain.cpp -o diffusionMain.out 

diffusionMain.cpp : DiffusionMain
	./DiffusionMain

DiffusionMain : DiffusionMain.hs
	ghc -O3 --make -i../../ DiffusionMain.hs

