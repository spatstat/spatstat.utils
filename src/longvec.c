#include <R.h>
#include <Rinternals.h>

/*

  longvec.c

  Operations for long vectors

  Cdiffdouble     'diff'
  Cdiffint
  Cnzdiff         'nonzero diff'

  $Revision: 1.4 $ $Date: 2025/12/29 05:36:18 $

 */

SEXP Cdiffdouble(SEXP rx) {
  // diff for long vectors of doubles
  R_xlen_t n = xlength(rx);
  R_xlen_t i;
  double *x = REAL(rx);
  double xcur, xprev;
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
  R_xlen_t n = xlength(rx);
  R_xlen_t i;
  // pointer to data
  int *x = INTEGER(rx);
  int xcur, xprev;
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
  R_xlen_t n = xlength(rx);
  R_xlen_t i;
  // pointer to data
  int *x = INTEGER(rx);
  int xcur, xprev;
  i = 1;
  xprev = x[i];
  for(i = 0; i < n; i++) {
    xcur = x[i];
    x[i] = (xcur == xprev)? 0:1;
    xprev = xcur;
  }
  return rx;
}

