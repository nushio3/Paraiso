all: lib kh.out

cuda: kh-cuda.out

kh.out: main-kh.cpp  dist/Hydro.cpp
	g++ main-kh.cpp dist/Hydro.cpp -O3  --openmp -Idist/ -o kh.out


kh-cuda.out: main-kh.cu  dist-cuda/Hydro.cu
	nvcc main-kh.cu dist-cuda/Hydro.cu -O3   -Idist-cuda/ -o kh-cuda.out -arch=sm_20

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

