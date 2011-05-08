#include "Life.hpp"

void Life::init () {
std::vector<int> a67(size());
int a70;
int a73;

for (int i = 0 ; i < size() ; ++i) {
const int a1_0_0 = ((i) % size0());
const int a3_0_0 = ((i / size0()) % size1());
const int a5_0_0 = 1;
const bool a7_0_0 = (a1_0_0 == a5_0_0);
const int a9_0_0 = 0;
const bool a11_0_0 = (a3_0_0 == a9_0_0);
const bool a13_0_0 = (a7_0_0 && a11_0_0);
const int a15_0_0 = 2;
const bool a17_0_0 = (a1_0_0 == a15_0_0);
const int a19_0_0 = 0;
const bool a21_0_0 = (a3_0_0 == a19_0_0);
const bool a23_0_0 = (a17_0_0 && a21_0_0);
const bool a25_0_0 = (a13_0_0 || a23_0_0);
const int a27_0_0 = 0;
const bool a29_0_0 = (a1_0_0 == a27_0_0);
const int a31_0_0 = 1;
const bool a33_0_0 = (a3_0_0 == a31_0_0);
const bool a35_0_0 = (a29_0_0 && a33_0_0);
const bool a37_0_0 = (a25_0_0 || a35_0_0);
const int a39_0_0 = 1;
const bool a41_0_0 = (a1_0_0 == a39_0_0);
const int a43_0_0 = 1;
const bool a45_0_0 = (a3_0_0 == a43_0_0);
const bool a47_0_0 = (a41_0_0 && a45_0_0);
const bool a49_0_0 = (a37_0_0 || a47_0_0);
const int a51_0_0 = 1;
const bool a53_0_0 = (a1_0_0 == a51_0_0);
const int a55_0_0 = 2;
const bool a57_0_0 = (a3_0_0 == a55_0_0);
const bool a59_0_0 = (a53_0_0 && a57_0_0);
const bool a61_0_0 = (a49_0_0 || a59_0_0);
const int a63_0_0 = 1;
const int a65_0_0 = 0;
a67[i] = (a61_0_0 ? a63_0_0 : a65_0_0);

}

for (int i = 0 ; i < size() ; ++i) {
cell()[i] = a67[i];

}

a70 = 0/*Reduce madayanen*/;


population() = a70;


a73 = 0;


generation() = a73;



return;
}

void Life::proceed () {
std::vector<int> a1(size());
int a3;
std::vector<int> a67(size());
int a69;
int a72;
int a74;

for (int i = 0 ; i < size() ; ++i) {
a1[i] = cell()[i];

}

a3 = generation();


for (int i = 0 ; i < size() ; ++i) {
const int a5_0_0 = a1[((((i) % size0()) + 1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + 1 + size1())%size1()))];
const int a7_0_0 = a1[((((i) % size0()) + 0 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + 1 + size1())%size1()))];
const int a9_0_0 = a1[((((i) % size0()) + -1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + 1 + size1())%size1()))];
const int a11_0_0 = a1[((((i) % size0()) + 1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + 0 + size1())%size1()))];
const int a13_0_0 = a1[((((i) % size0()) + -1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + 0 + size1())%size1()))];
const int a15_0_0 = a1[((((i) % size0()) + 1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + -1 + size1())%size1()))];
const int a17_0_0 = a1[((((i) % size0()) + 0 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + -1 + size1())%size1()))];
const int a19_0_0 = a1[((((i) % size0()) + -1 + size0())%size0()) + size0() * (((((i / size0()) % size1()) + -1 + size1())%size1()))];
const int a21_0_0 = (a5_0_0 + a7_0_0);
const int a23_0_0 = (a21_0_0 + a9_0_0);
const int a25_0_0 = (a23_0_0 + a11_0_0);
const int a27_0_0 = (a25_0_0 + a13_0_0);
const int a29_0_0 = (a27_0_0 + a15_0_0);
const int a31_0_0 = (a29_0_0 + a17_0_0);
const int a33_0_0 = (a31_0_0 + a19_0_0);
const int a35_0_0 = 0;
const bool a37_0_0 = (a1[i] == a35_0_0);
const int a39_0_0 = 3;
const bool a41_0_0 = (a33_0_0 == a39_0_0);
const bool a43_0_0 = (a37_0_0 && a41_0_0);
const int a45_0_0 = 1;
const bool a47_0_0 = (a1[i] == a45_0_0);
const int a49_0_0 = 2;
const bool a51_0_0 = (a33_0_0 >= a49_0_0);
const int a53_0_0 = 3;
const bool a55_0_0 = (a33_0_0 <= a53_0_0);
const bool a57_0_0 = (a51_0_0 && a55_0_0);
const bool a59_0_0 = (a47_0_0 && a57_0_0);
const bool a61_0_0 = (a43_0_0 || a59_0_0);
const int a63_0_0 = 1;
const int a65_0_0 = 0;
a67[i] = (a61_0_0 ? a63_0_0 : a65_0_0);

}

a69 = 0/*Reduce madayanen*/;


population() = a69;


a72 = 1;


a74 = (a3 + a72);


generation() = a74;


for (int i = 0 ; i < size() ; ++i) {
cell()[i] = a67[i];

}


return;
}


