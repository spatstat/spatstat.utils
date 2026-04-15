#include <R.h>
#include <Rinternals.h>

/*

  longvec.c

  Operations for long vectors

  Cdiffdouble     'diff'
  Cdiffint
  Cnzdiff         'nonzero diff'

  $Revision: 1.7 $ $Date: 2026/04/15 03:57:26 $

 */

SEXP Cdiffdouble(SEXP rx) {
  // diff for long vectors of double precision numeric values
  R_xlen_t n, i;
  double *x;
  double xcur, xprev;

  x = REAL(rx);
  n = xlength(rx);
  
  // compute successive differences, with leading 0
  i = 1;
  xprev = x[i];
  for(i = 0; i < n; i++) {
    xcur = x[i];
    x[i] = xcur - xprev;
    xprev = xcur;
  }
  return rx;
}

SEXP Cdiffint(SEXP rx) {
  // diff for long vectors of integers
  R_xlen_t n, i;
  int *x;
  int xcur, xprev;

  x = INTEGER(rx);
  n = xlength(rx);
  
  // compute successive differences, with leading 0
  i = 1;
  xprev = x[i];
  for(i = 0; i < n; i++) {
    xcur = x[i];
    x[i] = xcur - xprev;
    xprev = xcur;
  }
  return rx;
}

SEXP Cnzdiffint(SEXP rx) {
  // test whether diff() is non-zero (with leading TRUE)
  R_xlen_t n, i;
  int *x;
  int xcur, xprev;

  x = INTEGER(rx);
  n = xlength(rx);
  
  i = 1;
  xprev = x[i];
  for(i = 0; i < n; i++) {
    xcur = x[i];
    x[i] = (xcur == xprev)? 0:1;
    xprev = xcur;
  }
  return rx;
}

