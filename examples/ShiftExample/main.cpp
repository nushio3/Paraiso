#include <iostream>
#include <iomanip>
using namespace std;
#include "TableMaker.hpp"

TableMaker maker;

void dump () {
  cout << "index:";
  for (int x = -maker.om_lower_margin_0(); 
       x < maker.om_size_0() + maker.om_upper_margin_0(); 
       ++x) {
    cout << setw(7) << right << x;
  }
  cout << endl;
  cout << "value:";
  for (int x = -maker.om_lower_margin_0(); 
       x < maker.om_size_0() + maker.om_upper_margin_0(); 
       ++x) {
    cout << setw(7) << right << maker.table(x);
  }
  cout << endl;
  cout << endl;

}

int main () {

  maker.init();
  dump();
  maker.increment();
  dump();
  maker.calculate();
  dump();
  cout << "total: " << maker.total() << endl;
  cout << endl;

  
  return 0;
}
