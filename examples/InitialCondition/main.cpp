#include <fstream>
#include <iostream>
#include <iomanip>
using namespace std;
#include "dist/TableMaker.hpp"

int main () {
  TableMaker maker;
  maker.create();

  ofstream ofs("heart.txt");

  for (int y = 0; y < maker.om_size_1(); ++y) {
    for (int x = 0; x < maker.om_size_0(); ++x) {
      ofs << x << " " << y << " " << maker.table(x,y) << endl;
    }
    ofs << endl;
  }
  
  return 0;
}
