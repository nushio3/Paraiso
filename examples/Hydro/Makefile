all: lib #shock.out kh.out

hydro.o: dist/Hydro.hpp dist/Hydro.cpp
	g++ -c -O3 -Idist dist/Hydro.cpp -o hydro.o
shock.o: main-shock.cpp 
	g++ -c -O3 -Idist main-shock.cpp -o shock.o
kh.o: main-kh.cpp 
	g++ -c -O3 -Idist main-kh.cpp -o kh.o

shock.out: shock.o hydro.o
	g++ shock.o hydro.o -o shock.out
kh.out: kh.o hydro.o
	g++ kh.o hydro.o -o kh.out


lib:
	ghc -i../../ --make -O3  -fspec-constr-count=42 HydroMain.hs
	./HydroMain

recomp:
	ghc  -i../../ --make -O3 HydroMain.hs -fforce-recomp \
	-fspec-constr-count=16


clean:
	rm -fr *.o
	rm -fr *.out
	rm -fr dist/
	rm -f output/*.*

