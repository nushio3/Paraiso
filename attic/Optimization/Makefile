all: ghc dist/diffusion.cpp

dist/diffusion.cpp : DiffusionMain
	./DiffusionMain

ghc : DiffusionMain.hs
	ghc -O3 --make  -fspec-constr-count=42 -i../../ DiffusionMain.hs 

