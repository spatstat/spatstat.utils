#include <R.h>
#include <Rdefines.h>

/*

  fastinterv.c

  Fast version of findInterval when breaks are known to be evenly spaced
  and are known to embrace the data.

  fastCinterv    Intervals are left-open, right-closed ( ] except first one
                 "ceiling intervals"

  fastFinterv    Intervals are left-closed, right-open [ ) except last one
                 "floor intervals"

  $Revision: 1.6 $ $Date: 2025/04/04 06:58:25 $

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2018
  Licence: GNU Public Licence >= 2

*/

void fastCinterv(
		 double *x,           /* values to be classified */
		 int    *n,           /* number of x values */
		 double *brange,      /* range of breakpoints */
		 int    *nintervals,  /* number of intervals */
		 int    *y            /* resulting indices (start from 1) */
) {
  double bmin, bmax, db;
  int i, j, m, N;

  m = *nintervals;
  N = *n;

  bmin = brange[0];
  bmax = brange[1];
  db = (bmax - bmin)/m;
  
  for(i = 0; i < N; i++) {
    j = (int) ceil((x[i] - bmin)/db);
    if(j <= 0) { j = 1; } else if(j > m) { j = m; }
    y[i] = j;
  }
}

	       
void fastFinterv(
		 double *x,           /* values to be classified */
		 int    *n,           /* number of x values */
		 double *brange,      /* range of breakpoints */
		 int    *nintervals,  /* number of intervals */
		 int    *y            /* resulting indices (start from 1) */
) {
  double bmin, bmax, db;
  int i, j, m, N;

  m = *nintervals;
  N = *n;

  bmin = brange[0];
  bmax = brange[1];
  db = (bmax - bmin)/m;
  
  for(i = 0; i < N; i++) {
    j = 1 + (int) floor((x[i] - bmin)/db);
    if(j <= 0) { j = 1; } else if(j > m) { j = m; }
    y[i] = j;
  }
}

	       
