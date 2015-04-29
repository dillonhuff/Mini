#ifndef MINI_UTILITIES
#define MINI_UTILITIES

#include <math.h>

void rand_doubles(double* buf, int n) {
  int i;
  for (i = 0; i < n; i++) {
    buf[i] = rand() % 10;
  }
}

int test_buffer_diff_double(double* left, double* right, unsigned int len) {
  int i;
  for (i = 0; i < len; i++) {
    if (left[i] - right[i] != 0.0) {
      return 1;
    }
  }
  return 0;
}
#endif
