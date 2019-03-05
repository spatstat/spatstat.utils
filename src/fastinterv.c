#include <R.h>
#include <Rdefines.h>

/*

  fastinterv.c

  Fast version of findInterval when breaks are known to be evenly spaced
  and are known to embrace the data.

  $Revision: 1.3 $ $Date: 2018/12/18 02:43:11 $

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2018
  Licence: GNU Public Licence >= 2

*/

void fastinterv(x, n, brange, nintervals, y) 
     double *x, *brange;
     int *n, *nintervals;
     int *y;
{
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

	       
