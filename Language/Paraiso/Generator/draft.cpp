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


template <class T> T broadcast (const T& x) {
  return x;
}
template <class T> T reduce_sum (const thrust::device_vector<T> &xs) {
  return thurust::reduce(xs.begin(), xs.end(), 0, thrust::plus<T>());
}
template <class T> T reduce_min (const thrust::device_vector<T> &xs) {
  return *(thurust::min_element(xs.begin(), xs.end()));
}
template <class T> T reduce_max (const thrust::device_vector<T> &xs) {
  return *(thurust::max_element(xs.begin(), xs.end()));
}

