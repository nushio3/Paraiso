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

