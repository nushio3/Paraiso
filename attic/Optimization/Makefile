all: ghc dist/diffusion.cpp

dist/diffusion.cpp : DiffusionMain
	./DiffusionMain

ghc : DiffusionMain.hs
	ghc -O3 --make -i../../ DiffusionMain.hs

