#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "Hydro.hpp"

using namespace std;

const int W = 400, H = 1;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  for (int i = 0; i < W; ++i) {
    double u = sim.dR0() * (i+0.5 - sim.size0()/2) / sim.time();
    ofs << u << " "
        << sim.density()[i] << " "
        << sim.velocity0()[i] << " "
        << sim.velocity1()[i] << " "
        << sim.pressure()[i] << endl;
  }
}

int main () {
  Hydro sim(W, H);
  sim.time() = 0;
  sim.cfl() = 1;
  sim.extent0() = 2.0;
  sim.extent1() = 2.0;
  sim.dR0() = sim.extent0() / W;
  sim.dR1() = sim.extent1() / H;
  sim.init_shocktube();
  dump("begin.txt", sim);
  for (;;) {
    cerr << sim.time() << endl;
    sim.proceed();
    if (sim.time() >0.25) break;
  }
  dump("end.txt", sim);
}
