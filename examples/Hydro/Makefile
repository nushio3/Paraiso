all: shock.out kh.out

hydro.o: dist/Hydro.hpp dist/Hydro.cpp
	g++ -c -O3 -Idist dist/Hydro.cpp -o hydro.o
shock.o : main-shock.cpp 
	g++ -c -O3 -Idist -o shock.o
kh.o : main-kh.cpp 
	g++ -c -O3 -Idist -o kh.o

shock.out : shock.o hydro.o
	g++ shock.o hydro.o -o shock.out
kh.out : kh.o hydro.o
	g++ kh.o hydro.o -o kh.out


lib:
	runhaskell  -i../../ HydroMain.hs
# ghc -O3 --make -i../../ HydroMain.hs

clean:
	rm -fr *.o
	rm -fr *.out
	rm -fr dist/
	rm -f output/*.*
