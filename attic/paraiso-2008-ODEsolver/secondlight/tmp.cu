#include <iostream>
#include <cstdlib>
#include <cutil.h>
using namespace std;
double drand(double lo, double hi){
return lo + rand()/(double)RAND_MAX * (hi-lo);
}
__global__ void function_on_GPU_a5(float *a2_dev , float *a3_dev , float *a4_dev){
int a1 = blockIdx.x * gridDim.x + threadIdx.x;
float a2_sha;
a2_sha = a2_dev[a1];
float a3_sha;
a3_sha = a3_dev[a1];
float a4_sha;
a4_sha = a4_dev[a1];
__shared__ float a5_sha;
__shared__ float a6_sha;
__shared__ float a7_sha;
a5_sha = 10.0;
a6_sha = 28.0;
a7_sha = (8.0 / 3.0);
__shared__ float a8_sha;
a8_sha = 0.0;
__shared__ float a9_sha;
__shared__ float a10_sha;
__shared__ float a11_sha;
__shared__ float a12_sha;
__shared__ float a13_sha;
__shared__ float a14_sha;
__shared__ float a15_sha;
__shared__ float a16_sha;
__shared__ float a17_sha;
__shared__ float a18_sha;
__shared__ float a19_sha;
__shared__ float a20_sha;
__shared__ float a21_sha;
__shared__ float a22_sha;
__shared__ float a23_sha;
for(int a24 = 0 ; a24 < 655360 ; ++a24){
a9_sha = a2_sha;
a10_sha = a3_sha;
a11_sha = a4_sha;
a12_sha = ((0.0 - (a5_sha * a2_sha)) + (a5_sha * a3_sha));
a13_sha = (((0.0 - (a2_sha * a4_sha)) + (a6_sha * a2_sha)) - a3_sha);
a14_sha = ((a2_sha * a3_sha) - (a7_sha * a4_sha));
a8_sha = (a8_sha + 5.0e-3);
a2_sha = (a9_sha + (5.0e-3 * a12_sha));
a3_sha = (a10_sha + (5.0e-3 * a13_sha));
a4_sha = (a11_sha + (5.0e-3 * a14_sha));
a15_sha = ((0.0 - (a5_sha * a2_sha)) + (a5_sha * a3_sha));
a16_sha = (((0.0 - (a2_sha * a4_sha)) + (a6_sha * a2_sha)) - a3_sha);
a17_sha = ((a2_sha * a3_sha) - (a7_sha * a4_sha));
a2_sha = (a9_sha + (5.0e-3 * a15_sha));
a3_sha = (a10_sha + (5.0e-3 * a16_sha));
a4_sha = (a11_sha + (5.0e-3 * a17_sha));
a18_sha = ((0.0 - (a5_sha * a2_sha)) + (a5_sha * a3_sha));
a19_sha = (((0.0 - (a2_sha * a4_sha)) + (a6_sha * a2_sha)) - a3_sha);
a20_sha = ((a2_sha * a3_sha) - (a7_sha * a4_sha));
a8_sha = (a8_sha + 5.0e-3);
a2_sha = (a9_sha + (1.0e-2 * a18_sha));
a3_sha = (a10_sha + (1.0e-2 * a19_sha));
a4_sha = (a11_sha + (1.0e-2 * a20_sha));
a21_sha = ((0.0 - (a5_sha * a2_sha)) + (a5_sha * a3_sha));
a22_sha = (((0.0 - (a2_sha * a4_sha)) + (a6_sha * a2_sha)) - a3_sha);
a23_sha = ((a2_sha * a3_sha) - (a7_sha * a4_sha));
a2_sha = (a9_sha + (1.6666666666666666e-3 * (((a12_sha + (2.0 * a15_sha)) + (2.0 * a18_sha)) + a21_sha)));
a3_sha = (a10_sha + (1.6666666666666666e-3 * (((a13_sha + (2.0 * a16_sha)) + (2.0 * a19_sha)) + a22_sha)));
a4_sha = (a11_sha + (1.6666666666666666e-3 * (((a14_sha + (2.0 * a17_sha)) + (2.0 * a20_sha)) + a23_sha)));
}
a2_dev[a1] = a2_sha;
a3_dev[a1] = a3_sha;
a4_dev[a1] = a4_sha;
}
int main(int argc, char **argv){
dim3 grids(128);
dim3 threads(128);
float *a2 = (float*) malloc(sizeof(float)*16384);
float *a2_dev;
cudaMalloc((void**) &a2_dev ,sizeof(float)*16384);
float *a3 = (float*) malloc(sizeof(float)*16384);
float *a3_dev;
cudaMalloc((void**) &a3_dev ,sizeof(float)*16384);
float *a4 = (float*) malloc(sizeof(float)*16384);
float *a4_dev;
cudaMalloc((void**) &a4_dev ,sizeof(float)*16384);
for(int a1 = 0 ; a1 < 16384 ; ++a1){
a2[a1] = drand(10.0 , 20.0);
a3[a1] = drand(10.0 , 20.0);
a4[a1] = drand(10.0 , 20.0);
}
cudaMemcpy(a2_dev , a2,sizeof(float)*16384 , cudaMemcpyHostToDevice);
cudaMemcpy(a3_dev , a3,sizeof(float)*16384 , cudaMemcpyHostToDevice);
cudaMemcpy(a4_dev , a4,sizeof(float)*16384 , cudaMemcpyHostToDevice);
function_on_GPU_a5<<<grids,threads>>>(a2_dev , a3_dev , a4_dev);
cudaMemcpy(a2 , a2_dev,sizeof(float)*16384, cudaMemcpyDeviceToHost);
cudaMemcpy(a3 , a3_dev,sizeof(float)*16384, cudaMemcpyDeviceToHost);
cudaMemcpy(a4 , a4_dev,sizeof(float)*16384, cudaMemcpyDeviceToHost);
for(int a1 = 0 ; a1 < 16384 ; ++a1){
cout << a2[a1] << " " << a3[a1] << " " << a4[a1] << endl;
}
return 0;
}

