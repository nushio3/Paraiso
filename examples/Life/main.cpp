#include <cmath>
#include <cstdlib>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <unistd.h>

#include "Life.hpp"

using namespace std;

const string pat = " .':";

int main () {
  Life sim;
  const int W = sim.om_size_0();
  const int H = sim.om_size_1();

  sim.init();
  {
    ifstream ifs("init-pat.txt");
    int x,y;
    while (ifs >> x >> y) {
      sim.cell(x,y) = 1;
    }
  }

  int wait = 1000000;
  string buf; buf.resize((W+1)*H);
  for (int t = 0; ; ++t) {
    int j = 0;
    for (int y = 0; y < H; y+=2) {
      for (int x = 0; x < W; ++x) {
	buf[j++] = pat[2*sim.cell(x,y) + sim.cell(x,y+1)];
       }
       buf[j++]='\n';
    }
    ostringstream oss;
    oss << buf << "\n" 
	<< "generation: " << setw(6) << sim.generation() 
        << "  "
	<< "population: " << setw(4) << sim.population() 
        << endl;
    cout << oss.str() << flush;
    usleep(wait);
    wait = max(50000, int(wait*0.8));
    sim.proceed();
  }
}
