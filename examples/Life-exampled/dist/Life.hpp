#include <algorithm>
#include <cmath>
#include <vector>


class Life{
private:
int size0_;
int size1_;
int population_;
int generation_;
std::vector<int> cell_;

public:
const int size() const { return size0() * size1(); }
const int &size0() const { return size0_ ; }
const int &size1() const { return size1_ ; }
const int &population() const { return population_ ; }
const int &generation() const { return generation_ ; }
const std::vector<int> &cell() const { return cell_ ; }

public:
int &population() { return population_ ; }
int &generation() { return generation_ ; }
std::vector<int> &cell() { return cell_ ; }

public:
Life ( int size0,int size1 ): size0_(size0),size1_(size1),cell_(size()){}
void init ();
void proceed ();
void shift_x ();
void shift_y ();

};
