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

typedef double Real;

void dump (string fn, Hydro &sim) {
  ofstream ofs (fn.c_str()); 
  thrust::host_vector<Real> dens, vx, vy, p;

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

struct Field {
  virtual void at(const double t, const double x, const double y,
		  double &dens, double &vx, double &vy, double &p) = 0;
};


struct EntropyWave : public Field {
  virtual void at(const double t, const double x, const double y,
	  double &dens, double &vx, double &vy, double &p) {
    vx = 1;
    vy = 0;
    dens = 2 + sin(6.2832*(x - vx * t));
    p = 1;
  }
};

struct SoundWave : public Field {
  virtual void at(const double t, const double x, const double y,
	  double &dens, double &vx, double &vy, double &p) {
    const Real kGamma = 5.0 / 3.0;
    const Real soundSpeed = 1.0;
    const Real amplitude = 1e-3;

    const Real dens0 = kGamma;
    const Real p0 = 1;
    
    vx = amplitude * sin(6.2832*(x - soundSpeed * t));
    vy = 0;
    dens = dens0 + dens0/soundSpeed * vx;
    p = p0 + kGamma * p0 / soundSpeed * vx;
  }
};




void override (double t, Field &solution, Hydro &sim) {
  thrust::host_vector<Real> dens, vx, vy, p;

  const int iR = 10;
  
  dens = sim.static_7_density;
  vx   = sim.static_8_velocity0;
  vy   = sim.static_9_velocity1;
  p    = sim.static_10_pressure;

  for (int iy = 0; iy < H; ++iy) {
    for (int ix = 0; ix < W; ++ix) {
      double x = sim.static_3_dR0 * (ix+0.5);
      double y = sim.static_4_dR1 * (iy+0.5);
      int i = sim.memorySize0() * (iy+sim.lowerMargin1()) + ix + sim.lowerMargin0();
      
      if (t < 0.1 || ix < iR || iy < iR || ix >= W-iR || iy >= H-iR) {
	Real dens0, vx0, vy0, p0;
	solution.at(t,x,y,  dens0, vx0, vy0, p0);
	
	dens[i] = dens0;
	vx[i] =  vx0;
	vy[i] =  vy0;
	p[i]  =  p0;
      }
    }
  }

  sim.static_7_density   = dens ; 
  sim.static_8_velocity0 = vx   ;
  sim.static_9_velocity1 = vy   ;
  sim.static_10_pressure = p    ;

}



int main () {
  cudaSetDevice(2);
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

  SoundWave f;
  
  while (ctr <= 10) {
    double t = sim.static_1_time;
    cerr << sim.static_1_time << endl;
    if (!isfinite(t)) return -1;
    override(t, f, sim);
    sim.proceed();
    if (t > 0.1 * ctr) {
      sprintf(buf, "output-g%d/snapshot%04d.txt", antiAlias, ctr);
      dump(buf, sim);
      ++ctr;
    }
  }
}
