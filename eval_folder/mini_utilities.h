#ifndef MINI_UTILITIES
#define MINI_UTILITIES

#include <math.h>

void rand_doubles(double* buf, int n) {
  int i;
  for (i = 0; i < n; i++) {
    buf[i] = rand() % 10;
  }
}

#endif
