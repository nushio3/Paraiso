#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "Hydro.hpp"

using namespace std;

const int antiAlias = 1;
int W,H;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  for (int iy = antiAlias/2; iy < H; iy+=antiAlias) {
    for (int ix = antiAlias/2; ix < W; ix+=antiAlias) {
      double x = sim.dR0() * (ix+0.5);
      double y = sim.dR1() * (iy+0.5);
      ofs << x << " " << y << " "
          << sim.density(ix,iy) << " "
          << sim.velocity0(ix,iy) << " "
          << sim.velocity1(ix,iy) << " "
          << sim.pressure(ix,iy) << endl;
    }
    ofs << endl;
  }
}

int main () {
  Hydro sim;
  W = sim.om_size_0();
  H = sim.om_size_1();

  sim.time() = 0;
  sim.cfl() = 0.5;
  sim.extent0() = 1.0;
  sim.extent1() = 1.0;
  sim.dR0() = sim.extent0() / W;
  sim.dR1() = sim.extent1() / H;
  sim.init();
  char buf[256];
  sprintf(buf, "mkdir -p output%d", antiAlias);
  system(buf);
  int ctr = 0;
  while (ctr <= 100) {
    double t = sim.time();
    cerr << sim.time() << endl;
    if (!isfinite(t)) return -1;
    sim.proceed();
    if (t > 0.01 * ctr) {
      sprintf(buf, "output%d/snapshot%04d.txt", antiAlias, ctr);
      dump(buf, sim);
      ++ctr;
    }
  }
}
