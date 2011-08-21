#include <cmath>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "Life.hpp"

using namespace std;

const string pat = " .':";

int main () {
  Life sim;
  sim.init();
  const int W = sim.memorySize0();
  const int H = sim.memorySize1();
  int wait = 1000000;
  string buf; buf.resize((W+1)*H);
  for (int t = 0; t < 1500; ++t) {
    for (int y = 0; y < H/2; ++y) {
      for (int x = 0; x < W; ++x) {
	int i0 = (2*y)  *W+x;
        int i1 = (2*y+1)*W+x;
	int j = y*(W+1)+x;
	buf[j] = pat[2*sim.static_2_cell[i0] + sim.static_2_cell[i1]];
      }
      buf[y*(W+1)+W]='\n';
    }
    ostringstream oss;
    oss << buf << "\n" 
	<< "generation: " << t << endl;
    cout << oss.str() << flush;
    usleep(wait);
    wait = max(100000, int(wait*0.8));
    sim.proceed();
  }
}
