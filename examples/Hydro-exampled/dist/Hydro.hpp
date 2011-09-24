#pragma once


#include <algorithm>

#include <cmath>

#include <vector>


/*
lowerMargin = (Vec :~ 3) :~ 3
upperMargin = (Vec :~ 3) :~ 3
*/
class Hydro{
public: int static_0_generation;
public: float static_1_time;
public: float static_2_cfl;
public: float static_3_dR0;
public: float static_4_dR1;
public: float static_5_extent0;
public: float static_6_extent1;
public: std::vector<float>  static_7_density;
public: std::vector<float>  static_8_velocity0;
public: std::vector<float>  static_9_velocity1;
public: std::vector<float>  static_10_pressure;
public: std::vector<float>  manifest_0_8;
public: std::vector<float>  manifest_0_10;
public: std::vector<float>  manifest_0_12;
public: std::vector<float>  manifest_0_46;
public: std::vector<float>  manifest_0_52;
public: std::vector<float>  manifest_0_86;
public: std::vector<float>  manifest_0_93;
public: std::vector<float>  manifest_1_17;
public: std::vector<float>  manifest_1_19;
public: std::vector<float>  manifest_1_72;
public: std::vector<float>  manifest_1_74;
public: std::vector<float>  manifest_1_76;
public: std::vector<float>  manifest_1_308;
public: std::vector<float>  manifest_1_310;
public: std::vector<float>  manifest_1_312;
public: std::vector<float>  manifest_1_322;
public: std::vector<float>  manifest_1_324;
public: std::vector<float>  manifest_1_326;
public: std::vector<float>  manifest_1_336;
public: float manifest_1_338;
public: float manifest_1_340;
public: std::vector<float>  manifest_1_342;
public: std::vector<float>  manifest_1_344;
public: std::vector<float>  manifest_1_348;
public: std::vector<float>  manifest_1_352;
public: std::vector<float>  manifest_1_358;
public: std::vector<float>  manifest_1_362;
public: std::vector<float>  manifest_1_366;
public: std::vector<float>  manifest_1_370;
public: std::vector<float>  manifest_1_376;
public: std::vector<float>  manifest_1_1064;
public: std::vector<float>  manifest_1_1068;
public: std::vector<float>  manifest_1_1072;
public: std::vector<float>  manifest_1_1078;
public: std::vector<float>  manifest_1_1082;
public: std::vector<float>  manifest_1_1086;
public: std::vector<float>  manifest_1_1090;
public: std::vector<float>  manifest_1_1096;
public: std::vector<float>  manifest_1_1798;
public: std::vector<float>  manifest_1_1806;
public: std::vector<float>  manifest_1_1814;
public: std::vector<float>  manifest_1_1822;
public: std::vector<float>  manifest_1_3846;
public: std::vector<float>  manifest_1_3854;
public: std::vector<float>  manifest_1_3862;
public: std::vector<float>  manifest_1_3870;
public: std::vector<float>  manifest_1_3912;
public: std::vector<float>  manifest_1_3914;
public: std::vector<float>  manifest_1_3946;
public: std::vector<float>  manifest_1_3950;
public: float manifest_1_3952;
public:  Hydro () : static_0_generation(memorySize()),static_1_time(memorySize()),static_2_cfl(memorySize()),static_3_dR0(memorySize()),static_4_dR1(memorySize()),static_5_extent0(memorySize()),static_6_extent1(memorySize()),static_7_density(memorySize()),static_8_velocity0(memorySize()),static_9_velocity1(memorySize()),static_10_pressure(memorySize()),manifest_0_8(memorySize()),manifest_0_10(memorySize()),manifest_0_12(memorySize()),manifest_0_46(memorySize()),manifest_0_52(memorySize()),manifest_0_86(memorySize()),manifest_0_93(memorySize()),manifest_1_17(memorySize()),manifest_1_19(memorySize()),manifest_1_72(memorySize()),manifest_1_74(memorySize()),manifest_1_76(memorySize()),manifest_1_308(memorySize()),manifest_1_310(memorySize()),manifest_1_312(memorySize()),manifest_1_322(memorySize()),manifest_1_324(memorySize()),manifest_1_326(memorySize()),manifest_1_336(memorySize()),manifest_1_338(memorySize()),manifest_1_340(memorySize()),manifest_1_342(memorySize()),manifest_1_344(memorySize()),manifest_1_348(memorySize()),manifest_1_352(memorySize()),manifest_1_358(memorySize()),manifest_1_362(memorySize()),manifest_1_366(memorySize()),manifest_1_370(memorySize()),manifest_1_376(memorySize()),manifest_1_1064(memorySize()),manifest_1_1068(memorySize()),manifest_1_1072(memorySize()),manifest_1_1078(memorySize()),manifest_1_1082(memorySize()),manifest_1_1086(memorySize()),manifest_1_1090(memorySize()),manifest_1_1096(memorySize()),manifest_1_1798(memorySize()),manifest_1_1806(memorySize()),manifest_1_1814(memorySize()),manifest_1_1822(memorySize()),manifest_1_3846(memorySize()),manifest_1_3854(memorySize()),manifest_1_3862(memorySize()),manifest_1_3870(memorySize()),manifest_1_3912(memorySize()),manifest_1_3914(memorySize()),manifest_1_3946(memorySize()),manifest_1_3950(memorySize()),manifest_1_3952(memorySize()) {

}
public: int size ()  {
return 1048576;
}
public: int size0 ()  {
return 1024;
}
public: int size1 ()  {
return 1024;
}
public: int lowerMargin0 ()  {
return 3;
}
public: int lowerMargin1 ()  {
return 3;
}
public: int upperMargin0 ()  {
return 3;
}
public: int upperMargin1 ()  {
return 3;
}
public: int memorySize ()  {
return 1060900;
}
public: int memorySize0 ()  {
return 1030;
}
public: int memorySize1 ()  {
return 1030;
}
public: void Hydro_sub_0 (const float & a1, const float & a3, const float & a6, std::vector<float>  & a8, std::vector<float>  & a10, std::vector<float>  & a12) ;
public: void Hydro_sub_1 (const std::vector<float>  & a8, const std::vector<float>  & a10, const std::vector<float>  & a12, std::vector<float>  & a46, std::vector<float>  & a52, std::vector<float>  & a86, std::vector<float>  & a93) ;
public: void Hydro_sub_2 (const float & a13, const float & a15, const float & a65, const float & a67, const float & a70, std::vector<float>  & a17, std::vector<float>  & a19, std::vector<float>  & a72, std::vector<float>  & a74, std::vector<float>  & a76) ;
public: void Hydro_sub_3 (const std::vector<float>  & a1, const std::vector<float>  & a3, const std::vector<float>  & a5, const std::vector<float>  & a7, const std::vector<float>  & a72, const std::vector<float>  & a74, const std::vector<float>  & a76, std::vector<float>  & a308, std::vector<float>  & a310, std::vector<float>  & a312, std::vector<float>  & a322, std::vector<float>  & a324, std::vector<float>  & a326, std::vector<float>  & a344, std::vector<float>  & a348, std::vector<float>  & a352, std::vector<float>  & a358, std::vector<float>  & a362, std::vector<float>  & a366, std::vector<float>  & a370, std::vector<float>  & a376, std::vector<float>  & a1064, std::vector<float>  & a1068, std::vector<float>  & a1072, std::vector<float>  & a1078, std::vector<float>  & a1082, std::vector<float>  & a1086, std::vector<float>  & a1090, std::vector<float>  & a1096, std::vector<float>  & a1798, std::vector<float>  & a1806, std::vector<float>  & a1814, std::vector<float>  & a1822, std::vector<float>  & a3846, std::vector<float>  & a3854, std::vector<float>  & a3862, std::vector<float>  & a3870) ;
public: void Hydro_sub_4 (const std::vector<float>  & a17, const std::vector<float>  & a19, const std::vector<float>  & a308, const std::vector<float>  & a310, const std::vector<float>  & a312, const std::vector<float>  & a322, const std::vector<float>  & a324, const std::vector<float>  & a326, std::vector<float>  & a336) ;
public: void Hydro_sub_5 (const std::vector<float>  & a336, float & a338) ;
public: void Hydro_sub_6 (const float & a11, const float & a338, float & a340) ;
public: void Hydro_sub_7 (const float & a340, std::vector<float>  & a342) ;
public: void Hydro_sub_8 (const std::vector<float>  & a17, const std::vector<float>  & a19, const std::vector<float>  & a342, const std::vector<float>  & a344, const std::vector<float>  & a348, const std::vector<float>  & a352, const std::vector<float>  & a358, const std::vector<float>  & a362, const std::vector<float>  & a366, const std::vector<float>  & a370, const std::vector<float>  & a376, const std::vector<float>  & a1064, const std::vector<float>  & a1068, const std::vector<float>  & a1072, const std::vector<float>  & a1078, const std::vector<float>  & a1082, const std::vector<float>  & a1086, const std::vector<float>  & a1090, const std::vector<float>  & a1096, const std::vector<float>  & a1798, const std::vector<float>  & a1806, const std::vector<float>  & a1814, const std::vector<float>  & a1822, const std::vector<float>  & a3846, const std::vector<float>  & a3854, const std::vector<float>  & a3862, const std::vector<float>  & a3870, std::vector<float>  & a3912, std::vector<float>  & a3914, std::vector<float>  & a3946, std::vector<float>  & a3950) ;
public: void Hydro_sub_9 (const float & a9, const float & a340, float & a3952) ;
public: void init () ;
public: void proceed () ;
};
