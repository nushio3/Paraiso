all: main.out

main.out : main.cpp dist/Hydro.hpp dist/Hydro.cpp
	g++ -O3 -Idist main.cpp dist/Hydro.cpp -o main.out

lib:
	runhaskell  -i../../ HydroMain.hs
# ghc -O3 --make -i../../ HydroMain.hs

clean:
	rm -fr dist/
	rm -f output/*.*
