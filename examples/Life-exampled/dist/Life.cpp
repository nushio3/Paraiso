
#include "Life.hpp"




template <class T> T broadcast (const T& x) {
  return x;
}
template <class T> T reduce_sum (const std::vector<T> &xs) {
  T ret = 0;
  for (int i = 0; i < xs.size(); ++i) ret+=xs[i];
  return ret;
}
template <class T> T reduce_min (const std::vector<T> &xs) {
  T ret = xs[0];
  for (int i = 1; i < xs.size(); ++i) ret=std::min(ret,xs[i]);
  return ret;
}
template <class T> T reduce_max (const std::vector<T> &xs) {
  T ret = xs[0];
  for (int i = 1; i < xs.size(); ++i) ret=std::max(ret,xs[i]);
  return ret;
}

/*
lowerMargin = (Vec :~ 1) :~ 1
upperMargin = (Vec :~ 1) :~ 1
*/





















void Life::Life_sub_0 (std::vector<int>  & a99)  {
/*
lowerMargin = (Vec :~ 0) :~ 0
upperMargin = (Vec :~ 0) :~ 0
*/
for(int i = 0; (i) < (16900) ; ++(i)){
int addr_origin = i;
int a1_0_0 = ((i) % (130)) - (1);
int a3_0_0 = ((i) / (130)) - (1);
int a5_0_0 = 128;
int a7_0_0 = 128;
int a9_0_0 = 2;
int a11_0_0 = (a5_0_0) / (a9_0_0);
int a13_0_0 = 2;
int a15_0_0 = (a7_0_0) / (a13_0_0);
int a17_0_0 = (a1_0_0) - (a11_0_0);
int a19_0_0 = 1;
bool a21_0_0 = (a17_0_0) == (a19_0_0);
int a23_0_0 = (a3_0_0) - (a15_0_0);
int a25_0_0 = 0;
bool a27_0_0 = (a23_0_0) == (a25_0_0);
bool a29_0_0 = (a21_0_0) && (a27_0_0);
int a31_0_0 = (a1_0_0) - (a11_0_0);
int a33_0_0 = 2;
bool a35_0_0 = (a31_0_0) == (a33_0_0);
int a37_0_0 = (a3_0_0) - (a15_0_0);
int a39_0_0 = 0;
bool a41_0_0 = (a37_0_0) == (a39_0_0);
bool a43_0_0 = (a35_0_0) && (a41_0_0);
bool a45_0_0 = (a29_0_0) || (a43_0_0);
int a47_0_0 = (a1_0_0) - (a11_0_0);
int a49_0_0 = 0;
bool a51_0_0 = (a47_0_0) == (a49_0_0);
int a53_0_0 = (a3_0_0) - (a15_0_0);
int a55_0_0 = 1;
bool a57_0_0 = (a53_0_0) == (a55_0_0);
bool a59_0_0 = (a51_0_0) && (a57_0_0);
bool a61_0_0 = (a45_0_0) || (a59_0_0);
int a63_0_0 = (a1_0_0) - (a11_0_0);
int a65_0_0 = 1;
bool a67_0_0 = (a63_0_0) == (a65_0_0);
int a69_0_0 = (a3_0_0) - (a15_0_0);
int a71_0_0 = 1;
bool a73_0_0 = (a69_0_0) == (a71_0_0);
bool a75_0_0 = (a67_0_0) && (a73_0_0);
bool a77_0_0 = (a61_0_0) || (a75_0_0);
int a79_0_0 = (a1_0_0) - (a11_0_0);
int a81_0_0 = 1;
bool a83_0_0 = (a79_0_0) == (a81_0_0);
int a85_0_0 = (a3_0_0) - (a15_0_0);
int a87_0_0 = 2;
bool a89_0_0 = (a85_0_0) == (a87_0_0);
bool a91_0_0 = (a83_0_0) && (a89_0_0);
bool a93_0_0 = (a77_0_0) || (a91_0_0);
int a95_0_0 = 1;
int a97_0_0 = 0;
((a99)[addr_origin]) = ((a93_0_0) ? (a95_0_0) : (a97_0_0));
}
}
void Life::Life_sub_1 (const std::vector<int>  & a99, int & a102, int & a105)  {
(a102) = (reduce_sum(a99));
(a105) = (0);
}
void Life::Life_sub_2 (const std::vector<int>  & a1, std::vector<int>  & a67)  {
/*
lowerMargin = (Vec :~ 1) :~ 1
upperMargin = (Vec :~ 1) :~ 1
*/
for(int i = 0; (i) < (16384) ; ++(i)){
int addr_origin = ((((i) % (128)) + (1)) * (1)) + ((((i) / (128)) + (1)) * (130));
int a1_m1_m1 = (a1)[(addr_origin) + (-131)];
int a1_0_m1 = (a1)[(addr_origin) + (-130)];
int a1_1_m1 = (a1)[(addr_origin) + (-129)];
int a1_m1_0 = (a1)[(addr_origin) + (-1)];
int a1_0_0 = (a1)[(addr_origin) + (0)];
int a1_1_0 = (a1)[(addr_origin) + (1)];
int a1_m1_1 = (a1)[(addr_origin) + (129)];
int a1_0_1 = (a1)[(addr_origin) + (130)];
int a1_1_1 = (a1)[(addr_origin) + (131)];
int a5_0_0 = a1_1_1;
int a7_0_0 = a1_0_1;
int a9_0_0 = a1_m1_1;
int a11_0_0 = a1_1_0;
int a13_0_0 = a1_m1_0;
int a15_0_0 = a1_1_m1;
int a17_0_0 = a1_0_m1;
int a19_0_0 = a1_m1_m1;
int a21_0_0 = (a5_0_0) + (a7_0_0);
int a23_0_0 = (a21_0_0) + (a9_0_0);
int a25_0_0 = (a23_0_0) + (a11_0_0);
int a27_0_0 = (a25_0_0) + (a13_0_0);
int a29_0_0 = (a27_0_0) + (a15_0_0);
int a31_0_0 = (a29_0_0) + (a17_0_0);
int a33_0_0 = (a31_0_0) + (a19_0_0);
int a35_0_0 = 0;
bool a37_0_0 = (a1_0_0) == (a35_0_0);
int a39_0_0 = 3;
bool a41_0_0 = (a33_0_0) == (a39_0_0);
bool a43_0_0 = (a37_0_0) && (a41_0_0);
int a45_0_0 = 1;
bool a47_0_0 = (a1_0_0) == (a45_0_0);
int a49_0_0 = 2;
bool a51_0_0 = (a33_0_0) >= (a49_0_0);
int a53_0_0 = 3;
bool a55_0_0 = (a33_0_0) <= (a53_0_0);
bool a57_0_0 = (a51_0_0) && (a55_0_0);
bool a59_0_0 = (a47_0_0) && (a57_0_0);
bool a61_0_0 = (a43_0_0) || (a59_0_0);
int a63_0_0 = 1;
int a65_0_0 = 0;
((a67)[addr_origin]) = ((a61_0_0) ? (a63_0_0) : (a65_0_0));
}
}
void Life::Life_sub_3 (const int & a3, const std::vector<int>  & a67, int & a69, int & a74)  {
int a3_0_0 = a3;
(a69) = (reduce_sum(a67));
int a72_0_0 = 1;
(a74) = ((a3_0_0) + (a72_0_0));
}
void Life::init ()  {
Life_sub_0(manifest_0_99);
Life_sub_1(manifest_0_99, manifest_0_102, manifest_0_105);
(static_2_cell) = (manifest_0_99);
(static_0_population) = (manifest_0_102);
(static_1_generation) = (manifest_0_105);
}
void Life::proceed ()  {
Life_sub_2(static_2_cell, manifest_1_67);
Life_sub_3(static_1_generation, manifest_1_67, manifest_1_69, manifest_1_74);
(static_0_population) = (manifest_1_69);
(static_1_generation) = (manifest_1_74);
(static_2_cell) = (manifest_1_67);
}

