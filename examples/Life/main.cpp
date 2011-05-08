#include <cmath>
#include <cstdlib>
#include <iostream>
#include <string>
#include <unistd.h>

#include "Life.hpp"

using namespace std;

const int W = 60, H = 40;
const string pat = " .':";

int main () {
  Life sim(W, H);
  sim.init();
  int wait = 1000000;
  for (sim.generation() = 0; sim.generation() < 1000; sim.generation() ++) {
    string buf; buf.resize((W+1)*H);
    for (int y = 0; y < H/2; ++y) {
      for (int x = 0; x < W; ++x) {
	int i0 = (2*y)  *W+x;
        int i1 = (2*y+1)*W+x;
	int j = y*(W+1)+x;
	buf[j] = pat[2*sim.cell()[i0] + sim.cell()[i1]];
      }
      buf[y*(W+1)+W]='\n';
    }
    system("clear");
    cout << buf <<endl;
    usleep(wait);
    wait = max(100000, int(wait*0.9));
    sim.proceed();
  }
}
