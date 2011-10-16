#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <thrust/host_vector.h>
#include <unistd.h>

#include "get_time.h"
#include "Hydro.hpp"

using namespace std;

const int antiAlias = 1;
int W,H;

void check (Hydro &sim) {
  thrust::host_vector<float> dens, vx, vy, p;

  dens = sim.static_7_density;
  vx = sim.static_8_velocity0;
  vy = sim.static_9_velocity1;
  p = sim.static_10_pressure;

  bool hasNan = false;
  const float vThre = 1.0;
  int vxPosiCnt = 0, vxNegaCnt = 0;
  int vyPosiCnt = 0, vyNegaCnt = 0;
  
  for (int iy = antiAlias/2; iy < H; iy+=antiAlias) {
    for (int ix = antiAlias/2; ix < W; ix+=antiAlias) {
      double x = sim.static_3_dR0 * (ix+0.5);
      double y = sim.static_4_dR1 * (iy+0.5);
      int i = sim.memorySize0() * (iy+sim.lowerMargin1()) + ix + sim.lowerMargin0();
      hasNan = hasNan || not
	(isfinite(dens[i]) &&
	 isfinite(vx[i]) && 
	 isfinite(vy[i]) && 
	 isfinite(p[i]) );
      if (vx[i] >  vThre) ++vxPosiCnt; 
      if (vx[i] < -vThre) ++vxNegaCnt; 
      if (vy[i] >  vThre) ++vyPosiCnt; 
      if (vy[i] < -vThre) ++vyNegaCnt; 
    }
  }
  cerr << "nan? " << hasNan << endl;
  cerr << "velocity matrix" << endl;
  cerr <<
    vxPosiCnt<<"\t" <<
    vxNegaCnt<<"\t" <<
    vyPosiCnt<<"\t" <<
    vyNegaCnt<<endl;
}

int simulate () {
  Hydro sim;
  W = sim.size0();
  H = sim.size1();

  sim.static_1_time = 0;
  sim.static_2_cfl = 0.5;
  sim.static_5_extent0 = 1.0;
  sim.static_6_extent1 = 1.0;
  sim.static_3_dR0 = sim.static_5_extent0 / W;
  sim.static_4_dR1 = sim.static_6_extent1 / H;
  sim.init();
  int ctr = 0;
  while (ctr <= 100) {
    double t = sim.static_1_time;
    //cerr << sim.static_1_time << endl;
    if (!isfinite(t)) return -1;
    sim.proceed();
    if (t >= 0.01 * ctr) {
      check(sim);
      ++ctr;
    }
  }
  return 0;
}



int main (int argc, char **argv) {
  if (argc < 2) {
    cerr << "USAGE: " << argv[0] << " GPU_ID" << endl;
    return -1;
  }
  istringstream istr(argv[1]);
  int gpu_id;
  istr >> gpu_id;
  cudaSetDevice(gpu_id);
  simulate();
  return 0;
}
