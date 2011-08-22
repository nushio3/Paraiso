all: lib kh.out #shock.out


kh.out: main-kh.cpp  dist/Hydro.cpp
	g++ main-kh.cpp dist/Hydro.cpp -O3 -Idist/ -o kh.out

lib:
	ghc -i../../ --make -O3  -fspec-constr-count=42 HydroMain.hs
	time ./HydroMain

recomp:
	ghc  -i../../ --make -O3 HydroMain.hs -fforce-recomp \
	-fspec-constr-count=16


clean:
	rm -fr *.o
	rm -fr *.out
	rm -fr dist/
	rm -f output/*.*

