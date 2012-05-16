#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <thrust/host_vector.h>
#include <unistd.h>

#include "Hydro.hpp"

using namespace std;

const int antiAlias = 1;
int W,H;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  thrust::host_vector<float> dens, vx, vy, p;

  dens = sim.static_7_density;
  vx = sim.static_8_velocity0;
  vy = sim.static_9_velocity1;
  p = sim.static_10_pressure;

  for (int iy = antiAlias/2; iy < H; iy+=antiAlias) {
    for (int ix = antiAlias/2; ix < W; ix+=antiAlias) {
      double x = sim.static_3_dR0 * (ix+0.5);
      double y = sim.static_4_dR1 * (iy+0.5);
      int i = sim.memorySize0() * (iy+sim.lowerMargin1()) + ix + sim.lowerMargin0();
      ofs << x << " " << y << " "
          << dens[i] << " "
          << vx[i] << " "
          << vy[i] << " "
          << p[i] << endl;
    }
    ofs << endl;
  }
}

int main () {
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
  char buf[256];
  sprintf(buf, "mkdir -p output-g%d", antiAlias);
  system(buf);
  int ctr = 0;
  while (ctr <= 100) {
    double t = sim.static_1_time;
    cerr << sim.static_1_time << endl;
    if (!isfinite(t)) return -1;
    sim.proceed();
    if (t > 0.01 * ctr) {
      sprintf(buf, "output-g%d/snapshot%04d.txt", antiAlias, ctr);
      dump(buf, sim);
      ++ctr;
    }
  }
}
