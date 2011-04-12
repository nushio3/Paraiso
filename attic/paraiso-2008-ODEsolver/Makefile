SDK_INSTALL_PATH=/opt/NVIDIA_CUDA_SDK
CUDA_INSTALL_PATH=/usr/local/cuda


all:
	echo "make test, mandel, logistic int, lorenz"

test:
	ghc --make MainTest.hs -o gen.exe
mandel:
	ghc --make MainMandel.hs -o gen.exe
logistic:
	ghc --make MainLogistic.hs -o gen.exe
int:
	ghc --make MainIntegral.hs -o gen.exe
lorenz:
	ghc --make MainLorenz.hs -o gen.exe
fluid:
	ghc --make MainFluid.hs -o gen.exe

gpu: *.hs
	./gen.exe --cuda > tmp.cu
	indent -br < tmp.cu > main.cu
	nvcc -c main.cu -o main-gpu.o --host-compilation 'C++' -O3 -I  $(SDK_INSTALL_PATH)/common/inc  -I  $(CUDA_INSTALL_PATH)/include
	g++ main-gpu.o -o gpu.exe -L$(CUDA_INSTALL_PATH)/lib -L$(SDK_INSTALL_PATH)/lib -lcuda -lcudart -lGL  -lcutil
cpu: *.hs
	./gen.exe > tmp.cpp
	indent -br < tmp.cpp > main.cpp
	g++ main.cpp -O3 -o cpu.exe



run-gpu:
	LD_LIBRARY_PATH=/usr/local/cuda/lib ./gpu.exe > out-gpu.txt
	time LD_LIBRARY_PATH=/usr/local/cuda/lib ./gpu.exe > /dev/null
run-cpu:
	./cpu.exe > out-cpu.txt
	time ./cpu.exe > /dev/null

clear:
	rm -f *.exe *.manifest *.o *.hi *~
