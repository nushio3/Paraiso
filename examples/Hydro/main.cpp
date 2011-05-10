#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "Hydro.hpp"

using namespace std;

const int W = 128, H = 1;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  for (int i = 0; i < W; ++i) {
    double x = sim.dR0() * (i+0.5);
    ofs << x << " "
        << sim.density()[i] << " "
        << sim.velocity0()[i] << " "
        << sim.velocity1()[i] << " "
        << sim.pressure()[i] << endl;
  }
}

int main () {
  Hydro sim(W, H);
  sim.time() = 0;
  sim.extent0() = 1.0;
  sim.extent1() = 1.0;
  sim.dR0() = sim.extent0() / W;
  sim.dR1() = sim.extent1() / H;
  sim.init_shocktube();
  dump("begin.txt", sim);
  for (int i = 0 ; i < 1; ++i) {
    cerr << sim.time() << endl;
    sim.proceed();
  }
  dump("end.txt", sim);
}
