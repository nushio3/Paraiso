#include <cmath>
#include <fstream>
#include <iostream>
#include <vector>
using namespace std;
#include "dist/LinearWave.hpp"

double hogenyan = 0;


int main () {
  LinearWave sim;
  sim.initialize();
  ofstream ofs("field.txt");
  ofstream ofse("energy.txt");
  ofstream ofs_stat("stat.txt",ios::out | ios::app);

  double moment[3];
  vector<double> energies;

  const int n = sim.memorySize0();

  for (int t = 0; t < 300; ++t) {
    for (int t2 = 0; t2 < 10; ++t2) {
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
    
    energies.push_back(sim.energy);

    moment[0] += 1;
    moment[1] += sim.energy;
    moment[2] += sim.energy * sim.energy;
  }

  double mean = moment[1]/moment[0];
  double vari = moment[2]/moment[0] - mean*mean;
  //cerr << mean << " " << vari << endl;
  //ofs_stat << (n-2) << " " << sqrt(vari)/mean << endl;

  hogenyan = 0;
  for (int i = 0; i < energies.size();++i) {
    hogenyan = hogenyan + abs(energies[i] - mean);
  }
  ofs_stat << (n-2) << " " << hogenyan/mean/energies.size() << endl;
}
