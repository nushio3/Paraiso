#include <algorithm>
#include <cmath>
#include <vector>


class Hydro{
private:
int size0_;
int size1_;
int generation_;
double time_;
double cfl_;
double dR0_;
double dR1_;
double extent0_;
double extent1_;
std::vector<double> density_;
std::vector<double> velocity0_;
std::vector<double> velocity1_;
std::vector<double> pressure_;

public:
const int size() const { return size0() * size1(); }
const int &size0() const { return size0_ ; }
const int &size1() const { return size1_ ; }
const int &generation() const { return generation_ ; }
const double &time() const { return time_ ; }
const double &cfl() const { return cfl_ ; }
const double &dR0() const { return dR0_ ; }
const double &dR1() const { return dR1_ ; }
const double &extent0() const { return extent0_ ; }
const double &extent1() const { return extent1_ ; }
const std::vector<double> &density() const { return density_ ; }
const std::vector<double> &velocity0() const { return velocity0_ ; }
const std::vector<double> &velocity1() const { return velocity1_ ; }
const std::vector<double> &pressure() const { return pressure_ ; }

public:
int &generation() { return generation_ ; }
double &time() { return time_ ; }
double &cfl() { return cfl_ ; }
double &dR0() { return dR0_ ; }
double &dR1() { return dR1_ ; }
double &extent0() { return extent0_ ; }
double &extent1() { return extent1_ ; }
std::vector<double> &density() { return density_ ; }
std::vector<double> &velocity0() { return velocity0_ ; }
std::vector<double> &velocity1() { return velocity1_ ; }
std::vector<double> &pressure() { return pressure_ ; }

public:
Hydro ( int size0,int size1 ): size0_(size0),size1_(size1),density_(size()),velocity0_(size()),velocity1_(size()),pressure_(size()){}
void init_shocktube ();
void init_kh ();
void proceed ();

};
