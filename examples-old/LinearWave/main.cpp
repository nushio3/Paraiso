#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;
#include "dist/LinearWave.hpp"

int main () {
  LinearWave sim;
  sim.initialize();
  ofstream ofs("field.txt");
  ofstream ofse("energy.txt");
  for (int t = 0; t < 300; ++t) {
    for (int t2 = 0; t2 < 10; ++t2) {
      const int n = sim.memorySize0();
      sim.fieldF[0] = sim.fieldF[n-2];
      sim.fieldF[n-1] = sim.fieldF[1];
      sim.fieldG[0] = sim.fieldG[n-2];
      sim.fieldG[n-1] = sim.fieldG[1];
      sim.proceed();
    }
    for (int i = 0; i < sim.memorySize0(); ++i) {
      ofs << t << " " << i << " " << sim.fieldF[i] << " " << sim.fieldG[i] << endl;
    }
    ofs << endl;
    ofse << t << " " << sim.energy << endl;
  }
}
