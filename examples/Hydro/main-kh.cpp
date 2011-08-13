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
const int W = 200 * antiAlias, H = 200 * antiAlias;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  for (int iy = antiAlias/2; iy < H; iy+=antiAlias) {
    for (int ix = antiAlias/2; ix < W; ix+=antiAlias) {
      double x = sim.dR0() * (ix+0.5);
      double y = sim.dR0() * (iy+0.5);
      int i = W * iy + ix;
      ofs << x << " " << y << " "
          << sim.density()[i] << " "
          << sim.velocity0()[i] << " "
          << sim.velocity1()[i] << " "
          << sim.pressure()[i] << endl;
    }
    ofs << endl;
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
  char buf[256];
  sprintf(buf, "mkdir -p output%d", antiAlias);
  system(buf);
  int ctr = 0;
  while (ctr <= 1000) {
    cerr << sim.time() << endl;
    if (!isfinite(sim.time())) return -1;
    sim.proceed();
    if (sim.time() > 0.1 * ctr) {
      sprintf(buf, "output%d/snapshot%04d.txt", antiAlias, ctr);
      dump(buf, sim);
      ++ctr;
    }
  }
}
