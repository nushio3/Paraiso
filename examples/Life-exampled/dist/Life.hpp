#pragma once


#include <algorithm>

#include <cmath>

#include <vector>


/*
lowerMargin = (Vec :~ 1) :~ 1
upperMargin = (Vec :~ 1) :~ 1
*/
class Life{
public: int static_0_population;
public: int static_1_generation;
public: std::vector<int>  static_2_cell;
public: std::vector<int>  manifest_0_99;
public: int manifest_0_102;
public: int manifest_0_105;
public: std::vector<int>  manifest_1_67;
public: int manifest_1_69;
public: int manifest_1_74;
public:  Life () : static_0_population(memorySize()),static_1_generation(memorySize()),static_2_cell(memorySize()),manifest_0_99(memorySize()),manifest_0_102(memorySize()),manifest_0_105(memorySize()),manifest_1_67(memorySize()),manifest_1_69(memorySize()),manifest_1_74(memorySize()) {

}
public: int size ()  {
return 16384;
}
public: int size0 ()  {
return 128;
}
public: int size1 ()  {
return 128;
}
public: int lowerMargin0 ()  {
return 1;
}
public: int lowerMargin1 ()  {
return 1;
}
public: int upperMargin0 ()  {
return 1;
}
public: int upperMargin1 ()  {
return 1;
}
public: int memorySize ()  {
return 16900;
}
public: int memorySize0 ()  {
return 130;
}
public: int memorySize1 ()  {
return 130;
}
public: void Life_sub_0 (std::vector<int>  & a99) ;
public: void Life_sub_1 (const std::vector<int>  & a99, int & a102, int & a105) ;
public: void Life_sub_2 (const std::vector<int>  & a1, std::vector<int>  & a67) ;
public: void Life_sub_3 (const int & a3, const std::vector<int>  & a67, int & a69, int & a74) ;
public: void init () ;
public: void proceed () ;
};
