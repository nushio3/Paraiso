all: main.out

main.out : main.cpp dist/Life.hpp dist/Life.cpp
	g++ -O3 -Idist main.cpp dist/Life.cpp -o main.out


lib:
	runhaskell -i../../ LifeMain.hs

clean:
	rm -fr dist/
	rm -f output/*.*
