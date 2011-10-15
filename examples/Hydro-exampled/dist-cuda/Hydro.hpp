#pragma once


#include <thrust/device_vector.h>

#include <thrust/host_vector.h>

#include <thrust/functional.h>

#include <thrust/extrema.h>

#include <thrust/reduce.h>


/*
lowerMargin = (Vec :~ 3) :~ 3
upperMargin = (Vec :~ 3) :~ 3
*/
__global__ void Hydro_sub_0_inner (const float a1, const float a3, const float a6, float * a8, float * a10, float * a12) ;
__global__ void Hydro_sub_1_inner (const float * a8, const float * a10, const float * a12, float * a46, float * a52, float * a86, float * a93) ;
__global__ void Hydro_sub_2_inner (const float a13, const float a15, const float a65, const float a67, const float a70, float * a17, float * a19, float * a72, float * a74, float * a76) ;
__global__ void Hydro_sub_3_inner (const float * a1, const float * a3, const float * a5, const float * a7, const float * a72, const float * a74, const float * a76, float * a308, float * a310, float * a312, float * a322, float * a324, float * a326, float * a344, float * a348, float * a352, float * a358, float * a362, float * a366, float * a370, float * a376, float * a1064, float * a1068, float * a1072, float * a1078, float * a1082, float * a1086, float * a1090, float * a1096, float * a1798, float * a1806, float * a1814, float * a1822, float * a3846, float * a3854, float * a3862, float * a3870) ;
__global__ void Hydro_sub_4_inner (const float * a17, const float * a19, const float * a308, const float * a310, const float * a312, const float * a322, const float * a324, const float * a326, float * a336) ;
__global__ void Hydro_sub_7_inner (const float a340, float * a342) ;
__global__ void Hydro_sub_8_inner (const float * a17, const float * a19, const float * a342, const float * a344, const float * a348, const float * a352, const float * a358, const float * a362, const float * a366, const float * a370, const float * a376, const float * a1064, const float * a1068, const float * a1072, const float * a1078, const float * a1082, const float * a1086, const float * a1090, const float * a1096, const float * a1798, const float * a1806, const float * a1814, const float * a1822, const float * a3846, const float * a3854, const float * a3862, const float * a3870, float * a3912, float * a3914, float * a3946, float * a3950) ;
class Hydro{
public: int static_0_generation;
public: float static_1_time;
public: float static_2_cfl;
public: float static_3_dR0;
public: float static_4_dR1;
public: float static_5_extent0;
public: float static_6_extent1;
public: thrust::device_vector<float>  static_7_density;
public: thrust::device_vector<float>  static_8_velocity0;
public: thrust::device_vector<float>  static_9_velocity1;
public: thrust::device_vector<float>  static_10_pressure;
public: thrust::device_vector<float>  manifest_0_8;
public: thrust::device_vector<float>  manifest_0_10;
public: thrust::device_vector<float>  manifest_0_12;
public: thrust::device_vector<float>  manifest_0_46;
public: thrust::device_vector<float>  manifest_0_52;
public: thrust::device_vector<float>  manifest_0_86;
public: thrust::device_vector<float>  manifest_0_93;
public: thrust::device_vector<float>  manifest_1_17;
public: thrust::device_vector<float>  manifest_1_19;
public: thrust::device_vector<float>  manifest_1_72;
public: thrust::device_vector<float>  manifest_1_74;
public: thrust::device_vector<float>  manifest_1_76;
public: thrust::device_vector<float>  manifest_1_308;
public: thrust::device_vector<float>  manifest_1_310;
public: thrust::device_vector<float>  manifest_1_312;
public: thrust::device_vector<float>  manifest_1_322;
public: thrust::device_vector<float>  manifest_1_324;
public: thrust::device_vector<float>  manifest_1_326;
public: thrust::device_vector<float>  manifest_1_336;
public: float manifest_1_338;
public: float manifest_1_340;
public: thrust::device_vector<float>  manifest_1_342;
public: thrust::device_vector<float>  manifest_1_344;
public: thrust::device_vector<float>  manifest_1_348;
public: thrust::device_vector<float>  manifest_1_352;
public: thrust::device_vector<float>  manifest_1_358;
public: thrust::device_vector<float>  manifest_1_362;
public: thrust::device_vector<float>  manifest_1_366;
public: thrust::device_vector<float>  manifest_1_370;
public: thrust::device_vector<float>  manifest_1_376;
public: thrust::device_vector<float>  manifest_1_1064;
public: thrust::device_vector<float>  manifest_1_1068;
public: thrust::device_vector<float>  manifest_1_1072;
public: thrust::device_vector<float>  manifest_1_1078;
public: thrust::device_vector<float>  manifest_1_1082;
public: thrust::device_vector<float>  manifest_1_1086;
public: thrust::device_vector<float>  manifest_1_1090;
public: thrust::device_vector<float>  manifest_1_1096;
public: thrust::device_vector<float>  manifest_1_1798;
public: thrust::device_vector<float>  manifest_1_1806;
public: thrust::device_vector<float>  manifest_1_1814;
public: thrust::device_vector<float>  manifest_1_1822;
public: thrust::device_vector<float>  manifest_1_3846;
public: thrust::device_vector<float>  manifest_1_3854;
public: thrust::device_vector<float>  manifest_1_3862;
public: thrust::device_vector<float>  manifest_1_3870;
public: thrust::device_vector<float>  manifest_1_3912;
public: thrust::device_vector<float>  manifest_1_3914;
public: thrust::device_vector<float>  manifest_1_3946;
public: thrust::device_vector<float>  manifest_1_3950;
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
public: void Hydro_sub_0 (const float & a1, const float & a3, const float & a6, thrust::device_vector<float>  & a8, thrust::device_vector<float>  & a10, thrust::device_vector<float>  & a12) ;
public: void Hydro_sub_1 (const thrust::device_vector<float>  & a8, const thrust::device_vector<float>  & a10, const thrust::device_vector<float>  & a12, thrust::device_vector<float>  & a46, thrust::device_vector<float>  & a52, thrust::device_vector<float>  & a86, thrust::device_vector<float>  & a93) ;
public: void Hydro_sub_2 (const float & a13, const float & a15, const float & a65, const float & a67, const float & a70, thrust::device_vector<float>  & a17, thrust::device_vector<float>  & a19, thrust::device_vector<float>  & a72, thrust::device_vector<float>  & a74, thrust::device_vector<float>  & a76) ;
public: void Hydro_sub_3 (const thrust::device_vector<float>  & a1, const thrust::device_vector<float>  & a3, const thrust::device_vector<float>  & a5, const thrust::device_vector<float>  & a7, const thrust::device_vector<float>  & a72, const thrust::device_vector<float>  & a74, const thrust::device_vector<float>  & a76, thrust::device_vector<float>  & a308, thrust::device_vector<float>  & a310, thrust::device_vector<float>  & a312, thrust::device_vector<float>  & a322, thrust::device_vector<float>  & a324, thrust::device_vector<float>  & a326, thrust::device_vector<float>  & a344, thrust::device_vector<float>  & a348, thrust::device_vector<float>  & a352, thrust::device_vector<float>  & a358, thrust::device_vector<float>  & a362, thrust::device_vector<float>  & a366, thrust::device_vector<float>  & a370, thrust::device_vector<float>  & a376, thrust::device_vector<float>  & a1064, thrust::device_vector<float>  & a1068, thrust::device_vector<float>  & a1072, thrust::device_vector<float>  & a1078, thrust::device_vector<float>  & a1082, thrust::device_vector<float>  & a1086, thrust::device_vector<float>  & a1090, thrust::device_vector<float>  & a1096, thrust::device_vector<float>  & a1798, thrust::device_vector<float>  & a1806, thrust::device_vector<float>  & a1814, thrust::device_vector<float>  & a1822, thrust::device_vector<float>  & a3846, thrust::device_vector<float>  & a3854, thrust::device_vector<float>  & a3862, thrust::device_vector<float>  & a3870) ;
public: void Hydro_sub_4 (const thrust::device_vector<float>  & a17, const thrust::device_vector<float>  & a19, const thrust::device_vector<float>  & a308, const thrust::device_vector<float>  & a310, const thrust::device_vector<float>  & a312, const thrust::device_vector<float>  & a322, const thrust::device_vector<float>  & a324, const thrust::device_vector<float>  & a326, thrust::device_vector<float>  & a336) ;
public: void Hydro_sub_5 (const thrust::device_vector<float>  & a336, float & a338) ;
public: void Hydro_sub_6 (const float & a11, const float & a338, float & a340) ;
public: void Hydro_sub_7 (const float & a340, thrust::device_vector<float>  & a342) ;
public: void Hydro_sub_8 (const thrust::device_vector<float>  & a17, const thrust::device_vector<float>  & a19, const thrust::device_vector<float>  & a342, const thrust::device_vector<float>  & a344, const thrust::device_vector<float>  & a348, const thrust::device_vector<float>  & a352, const thrust::device_vector<float>  & a358, const thrust::device_vector<float>  & a362, const thrust::device_vector<float>  & a366, const thrust::device_vector<float>  & a370, const thrust::device_vector<float>  & a376, const thrust::device_vector<float>  & a1064, const thrust::device_vector<float>  & a1068, const thrust::device_vector<float>  & a1072, const thrust::device_vector<float>  & a1078, const thrust::device_vector<float>  & a1082, const thrust::device_vector<float>  & a1086, const thrust::device_vector<float>  & a1090, const thrust::device_vector<float>  & a1096, const thrust::device_vector<float>  & a1798, const thrust::device_vector<float>  & a1806, const thrust::device_vector<float>  & a1814, const thrust::device_vector<float>  & a1822, const thrust::device_vector<float>  & a3846, const thrust::device_vector<float>  & a3854, const thrust::device_vector<float>  & a3862, const thrust::device_vector<float>  & a3870, thrust::device_vector<float>  & a3912, thrust::device_vector<float>  & a3914, thrust::device_vector<float>  & a3946, thrust::device_vector<float>  & a3950) ;
public: void Hydro_sub_9 (const float & a9, const float & a340, float & a3952) ;
public: void init () ;
public: void proceed () ;
};
