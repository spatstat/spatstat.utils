/*

  matchpoints.c

  $Revision: 1.2 $  $Date: 2018/12/18 02:43:11 $

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2018
  Licence: GNU Public Licence >= 2

  Cmatchxy       Find matches between two sets of points   

*/

#include <math.h>
#include <R_ext/Utils.h>
#include "chunkloop.h"

/*

  Cmatchxy

  Find matches between two lists of points

 */

void Cmatchxy(na, xa, ya, nb, xb, yb, match)
     /* inputs */
     int *na, *nb;
     double *xa, *ya, *xb, *yb;
     /* output */
     int *match; 
     /* match[i] = j+1 if xb[j], yb[j] matches xa[i], ya[i] */
     /* match[i] = 0   if no such point matches xa[i], ya[i] */
{ 
  int i, j, Na, Nb, maxchunk; 
  double xai, yai;

  Na = *na;
  Nb = *nb;

  OUTERCHUNKLOOP(i, Na, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(i, Na, maxchunk, 16384) {
      xai = xa[i];
      yai = ya[i];
      match[i] = 0;
      for (j=0; j < Nb; j++) {
	if(xai == xb[j] && yai == yb[j]) {
	  match[i] = j+1;
	  break;
	}
      }
    }
  }
}
