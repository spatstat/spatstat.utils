/* 

   revcum.c

   $Revision: 1.4 $  $Date: 2018/12/18 02:43:11 $

   Reverse cumulative sums

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2018
  Licence: GNU Public Licence >= 2

*/

void drevcumsum(double *x, int *nx) {
  int i;
  double sumx;
  double *xp;
  
  i = *nx - 1;
  xp = x + i;
  sumx = *xp;
  while(i > 0) {
    --i;
    --xp;
    sumx += *xp;
    *xp = sumx;
  }
}

void irevcumsum(int *x, int *nx) {
  int i;
  int sumx;
  int *xp;
  
  i = *nx - 1;
  xp = x + i;
  sumx = *xp;
  while(i > 0) {
    --i;
    --xp;
    sumx += *xp;
    *xp = sumx;
  }
}
