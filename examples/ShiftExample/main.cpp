#include <iostream>
#include <iomanip>
using namespace std;
#include "TableMaker.hpp"

int main () {
  TableMaker maker;

  maker.create();

  for (int x = 0; x < maker.om_size_0(); ++x) {
    cout << setw(4) << right << maker.table(x);
  }
  cout << endl;

  cout << "total: " << maker.total() << endl;
  
  return 0;
}
