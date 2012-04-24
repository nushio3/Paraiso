#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

int main () {
  const int N = 100;
  vector<double> f(N),u(N), new_f(N),new_u(N);
  const double c = 1.0;  
  const double dx = 2 * 3.1415926536 / N;
  const double dt = 1.0 * dx / c;
  for (int i = 0; i < N; ++i) {
    double x = dx*(i+0.5);
    f[i] = sin(x);
    u[i] = cos(3*x);
  }

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
      new_f[i] = f[i] + dt * u[i];
    }
    swap(new_f,f);

    for (int i = 0; i < N; ++i) {
      int im = (i+N-1)%N;
      int ip = (i+1)%N;
      new_u[i] = u[i] + dt*c*c/dx/dx*(f[im]+f[ip]-2*f[i]);
    }
    swap(new_u,u);
  }
}
