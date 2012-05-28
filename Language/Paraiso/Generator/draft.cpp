#put this into ruby!
cpu = (<<ENDOFLIB);

template <class T> T om_broadcast (const T& x) {
  return x;
}
template <class T> T om_reduce_sum (const std::vector<T> &xs) {
  T ret = 0;
  for (int i = 0; i < xs.size(); ++i) ret+=xs[i];
  return ret;
}
template <class T> T om_reduce_min (const std::vector<T> &xs) {
  T ret = xs[0];
  for (int i = 1; i < xs.size(); ++i) ret=std::min(ret,xs[i]);
  return ret;
}
template <class T> T om_reduce_max (const std::vector<T> &xs) {
  T ret = xs[0];
  for (int i = 1; i < xs.size(); ++i) ret=std::max(ret,xs[i]);
  return ret;
}

ENDOFLIB
;

gpu = (<<ENDOFLIB);

template <class T> __device__ __host__ T om_broadcast (const T& x) {
  return x;
}
template <class T> T om_reduce_sum (const thrust::thrust_vector<T> &xs) {
  return thrust::reduce(xs.device_vector().begin(), xs.device_vector().end(), 0, thrust::plus<T>());
}
template <class T> T om_reduce_min (const thrust::thrust_vector<T> &xs) {
  return *(thrust::min_element(xs.device_vector().begin(), xs.device_vector().end()));
}
template <class T> T om_reduce_max (const thrust::thrust_vector<T> &xs) {
  return *(thrust::max_element(xs.device_vector().begin(), xs.device_vector().end()));
}

ENDOFLIB

thrustVector = (<<ENDOFLIB);
#pragma once

#include <thrust/device_vector.h>
#include <thrust/host_vector.h>
#include <thrust/device_ptr.h>
#include <iostream>

namespace thrust {

template <class T>
class thrust_vector {
public:
  enum NewerFlag {
    kHost,
    kDevice,
    kBoth
  };
private:
  mutable thrust::host_vector<T> hv;
  mutable thrust::device_vector<T> dv;
  mutable NewerFlag newer_;
public:  

  typedef typename thrust::host_vector<T>::size_type  size_type;
  typedef T value_type;

  thrust_vector () : newer_(kBoth) {}
  thrust_vector (size_type n, const value_type &value = value_type()) :
    hv(n, value), dv(n, value), newer_(kBoth) {}
  thrust_vector (const thrust_vector<T> &v) :
    hv(v.hv), dv(v.dv), newer_(kBoth) {}

  const size_type size() const { return hv.size(); }

  void bring_device () const {
    if (kHost == newer_) {
      dv = hv;
      newer_ = kBoth;
    }
  }
  void bring_host () const {
    if (kDevice == newer_) {
      hv = dv;
      newer_ = kBoth;
    }
  }

  thrust::host_vector<T> &host_vector() const {
    bring_host();
    return hv;
  }
  thrust::device_vector<T> &device_vector() const {
    bring_device();
    return dv;
  }
  
  const T& operator[](const size_type n) const {
    bring_host();
    return hv[n];
  }
  T& operator[](const size_type n) {
    bring_host();
    newer_ = kHost;
    return hv[n];
  }
  T* unsafe_raw_device () const {
    return thrust::raw_pointer_cast(&*dv.begin());
  }
  T* unsafe_raw_host () const {
    return &hv[0];
  }
  void unsafe_set_newer (NewerFlag n) {
    newer_ = n;
  }
  
  T* raw () const {
    bring_device();
    newer_ = kDevice;
    return thrust::raw_pointer_cast(&*dv.begin());
  }
  typename thrust::device_vector<T>::iterator device_begin () {
    bring_device();
    return dv.begin();
  }
  typename thrust::device_vector<T>::iterator device_end () {
    bring_device();
    return dv.end();
  }
};

template<class T>
T* raw(thrust::device_vector<T> &dv) {
  return thrust::raw_pointer_cast(&*dv.begin());
}

template<class T>
T* raw(thrust::host_vector<T> &hv) {
  return &hv[0];
}

template<class T>
T* raw(const thrust::thrust_vector<T> &tv) {
  return tv.raw();
}

}
ENDOFLIB

puts 'cpuLib = ' + cpu.inspect
puts 'gpuLib = ' + gpu.inspect
puts 'thrustVectorLib = ' + thrustVector.inspect

