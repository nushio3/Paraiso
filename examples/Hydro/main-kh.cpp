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

const int W = 200, H = 200;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  for (int iy = 0; iy < H; ++iy) {
    for (int ix = 0; ix < W; ++ix) {
      double x = sim.dR0() * (ix+0.5);
      double y = sim.dR0() * (iy+0.5);
      int i = W * iy + ix;
      ofs << x << " " << y << " "
          << sim.density()[i] << " "
          << sim.velocity0()[i] << " "
          << sim.velocity1()[i] << " "
          << sim.pressure()[i] << endl;
    }
  }
}

int main () {
  Hydro sim(W, H);
  sim.time() = 0;
  sim.cfl() = 0.5;
  sim.extent0() = 1.0;
  sim.extent1() = 1.0;
  sim.dR0() = sim.extent0() / W;
  sim.dR1() = sim.extent1() / H;
  sim.init_kh();
  int ctr = 0;
  system("mkdir -p output");
  while (ctr <= 100) {
    cerr << sim.time() << endl;
    if (!isfinite(sim.time())) return -1;
    sim.proceed();
    if (sim.time() > 0.1 * ctr) {
      char buf[256];
      sprintf(buf, "output/snapshot%04d", ctr);
      dump(buf, sim);
      ++ctr;
    }
  }
}
