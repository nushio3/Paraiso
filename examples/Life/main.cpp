#include <cstdlib>
#include <iostream>
#include <string>
#include <unistd.h>

#include "Life.hpp"

using namespace std;

const int W = 60, H = 30;

int main () {
  Life sim(W, H);
  sim.init();
  for (sim.generation() = 0; sim.generation() < 1000; sim.generation() ++) {
    string buf; buf.resize((W+1)*H);
    for (int y = 0; y < H; ++y) {
      for (int x = 0; x < W; ++x) {
	int i = y*W+x;
	int j = y*(W+1)+x;
	buf[j] = sim.cell()[i] ? '8' : '.';
      }
      buf[y*(W+1)+W]='\n';
    }
    system("clear");
    cout << buf <<endl;
    usleep(100000);
    sim.proceed();
  }
}
