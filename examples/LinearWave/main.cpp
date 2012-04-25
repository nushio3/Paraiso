#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;
#include "dist/LinearWave.hpp"

int main () {
  LinearWave sim;
  sim.initialize();
  ofstream ofs("out.txt");
  for (int t = 0; t < 300; ++t) {
    for (int t2 = 0; t2 < 10; ++t2) sim.proceed();
    for (int i = 0; i < sim.memorySize0(); ++i) {
      ofs << t << " " << i << " " << sim.static_0_fieldF[i] << " " << sim.static_0_fieldG[i] << endl;
    }
    ofs << endl;
  }
}
