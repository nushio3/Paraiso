#include "Life.hpp"

int main () {
  Life sim(100, 100);
  sim.init();
  for (sim.generation() = 0; sim.generation() < 100; sim.generation() ++) {
    sim.proceed();
  }
}
