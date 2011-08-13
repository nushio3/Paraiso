all: dist/diffusion.cpp

dist/diffusion.cpp : DiffusionMain
	./DiffusionMain

DiffusionMain : DiffusionMain.hs
	ghc -O3 --make -i../../ DiffusionMain.hs

