#ifndef MINI_UTILITIES
#define MINI_UTILITIES

#include <math.h>
#include <stdio.h>

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

void print_mat_double(double* a,
		      unsigned int a_nr, unsigned int a_nc,
		      unsigned int a_rs, unsigned int a_cs) {
  unsigned int i, j;
  for (i = 0; i < a_nr; i++) {
    for (j = 0; j < a_nc; j++) {
      printf("%f ", a[i*a_rs + j*a_cs]);
    }
    printf("\n");
  }
}

unsigned long long rdtsc()
{
   unsigned long long int x;
   unsigned a, d;

   __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));

   return ((unsigned long long)a) | (((unsigned long long)d) << 32);
}

void print_first_byte(void* p) {
  unsigned char* c = (unsigned char*) p;
  printf("%c\n", c[0]);
}

#endif
