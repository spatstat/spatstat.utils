/*

  matchindices.c

  $Revision$  $Date$

  Cmatch2int    Find matches between two lists of pairs of integers
  Cmatch3int    Find matches between two lists of triples of integers

*/

#include <math.h>
#include <R_ext/Utils.h>
#include "chunkloop.h"

/*

  Cmatch2int

  Find matches between two lists of pairs of integers

  Each list sorted by order(x,y)

 */

void Cmatch2int(na, xa, ya, nb, xb, yb, match)
  /* inputs */
  int *na, *nb;
  int *xa, *ya; /* sorted into increasing order of (xa, ya) */
  int *xb, *yb; /* sorted into increasing order of (xb, yb) */
  /* output */
  int *match; 
  /* match[i] = j+1 if xb[j], yb[j] matches xa[i], ya[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i] */
{ 
  int i, j, Na, Nb, maxchunk; 
  int xai, yai;

  Na = *na;
  Nb = *nb;

  j = 0;
  
  OUTERCHUNKLOOP(i, Na, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(i, Na, maxchunk, 16384) {
      xai = xa[i];
      yai = ya[i];
      match[i] = 0;
      while(j < Nb && xb[j] < xai) ++j;
      while(j < Nb && xb[j] == xai && yb[j] < yai) ++j;
      if(j < Nb && xb[j] == xai && yb[j] == yai) 
	match[i] = j+1;
      if(j >= Nb) return;
    }
  }
}


/*

  Cmatch3int

  Find matches between two lists of triples of integers

  Each list sorted by order(x,y,z)

 */

void Cmatch3int(na, xa, ya, za, nb, xb, yb, zb, match)
  /* inputs */
  int *na, *nb;
  int *xa, *ya, *za; /* sorted into increasing order of (xa, ya, za) */
  int *xb, *yb, *zb; /* sorted into increasing order of (xb, yb, zb) */
  /* output */
  int *match; 
  /* match[i] = j+1 if xb[j], yb[j], zb[j] matches xa[i], ya[i], za[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i], za[i] */
{ 
  int i, j, Na, Nb, maxchunk; 
  int xai, yai, zai;

  Na = *na;
  Nb = *nb;

  j = 0;
  
  OUTERCHUNKLOOP(i, Na, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(i, Na, maxchunk, 16384) {
      xai = xa[i];
      yai = ya[i];
      zai = za[i];
      match[i] = 0;
      while(j < Nb && xb[j] < xai) ++j;
      while(j < Nb && xb[j] == xai && yb[j] < yai) ++j;
      while(j < Nb && xb[j] == xai && yb[j] == yai && zb[j] < zai) ++j;
      if(j < Nb && xb[j] == xai && yb[j] == yai && zb[j] == zai) 
	match[i] = j+1;
      if(j >= Nb) return;
    }
  }
}
