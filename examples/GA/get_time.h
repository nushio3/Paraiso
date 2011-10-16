#pragma once
#include <sys/time.h>

template<class T> T get_time() {
 timeval tm;
 gettimeofday(&tm, NULL);
 return T(tm.tv_sec) + T(1e-6)*T(tm.tv_usec);
}

