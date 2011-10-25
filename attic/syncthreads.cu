#include <thrust/device_vector.h>
#include <iostream>
using namespace std;

const int N = 100;

__global__
void calculate (float *px, float *py, float *pz) {
  float ctr = 0;
  for (int tid = threadIdx.x; tid < N; tid+= blockDim.x) {
    py[tid] = sqrt(px[tid]);
    if (tid%2==0) __syncthreads();
    pz[tid] = ctr++;
    __syncthreads();
  }
}

int main () {
  thrust::device_vector<float> xs(N), ys(N), zs(N);
  for (int i=0; i<N; i++) {
    xs[i] = i;
  }
  calculate<<<1, 32>>>
    (thrust::raw_pointer_cast(&*xs.begin()),
     thrust::raw_pointer_cast(&*ys.begin()),
     thrust::raw_pointer_cast(&*zs.begin())
     );
  for (int i = 0; i < N; i++) {
    cout<<"sqrt "<<xs[i]<<" is "<<ys[i] << " updated at " << zs[i]<<endl;
  }
}            
