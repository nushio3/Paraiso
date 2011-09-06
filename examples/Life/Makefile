all: main.out
cuda: main-cuda.out

main.out : main.cpp dist/Life.hpp dist/Life.cpp
	g++ -O3 -Idist main.cpp dist/Life.cpp -o main.out

main-cuda.out : main.cu dist-cuda/Life.hpp dist-cuda/Life.cu
	nvcc -O3 -Idist-cuda main.cu dist-cuda/Life.cu -o main-cuda.out


lib:
	runhaskell -i../../ LifeMain.hs

clean:
	rm -fr dist/
	rm -f output/*.*
