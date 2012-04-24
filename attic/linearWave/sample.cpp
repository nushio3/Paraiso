#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

int main () {
  const int N = 100;
  vector<double> f(N),u(N);
  const double c = 1.0;  
  const double dx = 1.0 / N;
  const double dt = 0.5 * dx / c;
  for (int i = 0; i < N/3; ++i) {f[i] = 1.0;}

  int ctr = 0;
  for (double t = 0; t < 3.0; t+=dt) {
    if (t >= 0.1 * ctr) {
      ostringstream fn; fn << (13000 + ctr) << ".txt";
      ofstream ofs(fn.str().c_str());
      for (int i = 0; i < N; ++i) {
        ofs << dx*(i+0.5) << " " << f[i] << " " << u[i] << endl;
      }
      ctr++;
    }
    for (int i = 0; i < N; ++i) {
      f[i] += dt * u[i];
    }

    for (int i = 0; i < N; ++i) {
      int im = (i+N-1)%N;
      int ip = (i+1)%N;
      u[i] -= dt*c*c/dx/dx*(f[im]+f[ip]-2*f[i]);
    }
  }
}
