#include <iostream>
#include <iomanip>
using namespace std;
#include "dist/TableMaker.hpp"

int main () {
  TableMaker maker;
  maker.create();

  for (int y = 0; y < maker.om_size_1(); ++y) {
    for (int x = 0; x < maker.om_size_0(); ++x) {
      cout << setw(4) << right << maker.table(x,y);
    }
    cout << endl;
  }
  
  cout << "total: " << maker.total() << endl;
  
  return 0;
}
