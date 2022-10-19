/*

  matchindices.c

  $Revision: 1.3 $  $Date: 2022/10/19 03:35:51 $

  CSmatch2int    Find matches between two sorted lists of pairs of integers
  CSmatch3int    Find matches between two sorted lists of triples of integers

  CUmatch2int    Find matches between two UNsorted lists of pairs of integers
  CUmatch3int    Find matches between two UNsorted lists of triples of integers

*/

#include <math.h>
#include <R_ext/Utils.h>
#include "chunkloop.h"

/* ................ unsorted ............................ */
/* ........ Behaviour equivalent to match() ............. */


/*

  CUmatch2int

  Find matches between two unsorted lists of pairs of integers

 */

void CUmatch2int(
  /* inputs */
  int *na,
  int *xa,
  int *ya, 
  int *nb,
  int *xb,
  int *yb,
  /* output */
  int *match 
  /* match[i] = j+1 if xb[j], yb[j] matches xa[i], ya[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i] */
) { 
  int i, j, Na, Nb, maxchunk; 
  int xai, yai;

  Na = *na;
  Nb = *nb;

  OUTERCHUNKLOOP(i, Na, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(i, Na, maxchunk, 16384) {
      xai = xa[i];
      yai = ya[i];
      match[i] = 0;
      for(j = 0; j < Nb; j++) {
	if(xb[j] == xai && yb[j] == yai) {
	  match[i] = j+1;
	  break;
	}
      }
    }
  }
}


/*

  CUmatch3int

  Find matches between two unsorted lists of triples of integers

 */

void CUmatch3int(
  /* inputs */
  int *na,
  int *xa,
  int *ya,
  int *za, 
  int *nb,
  int *xb,
  int *yb,
  int *zb, 
  /* output */
  int *match 
  /* match[i] = j+1 if xb[j], yb[j], zb[j] matches xa[i], ya[i], za[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i], za[i] */
) { 
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
      for(j = 0; j < Nb; j++) {
	if(xb[j] == xai && yb[j] == yai && zb[j] == zai) {
	  match[i] = j+1;
	  break;
	}
      }
    }
  }
}


/* ................ sorted ............................ */

/*

  CSmatch2int

  Find matches between two lists of pairs of integers

  Each list sorted by order(x,y)

 */

void CSmatch2int(
  /* inputs */
  int *na,
  int *xa,
  int *ya, /* sorted into increasing order of (xa, ya) */
  int *nb,
  int *xb,
  int *yb, /* sorted into increasing order of (xb, yb) */
  /* output */
  int *match 
  /* match[i] = j+1 if xb[j], yb[j] matches xa[i], ya[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i] */
) { 
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

  CSmatch3int

  Find matches between two lists of triples of integers

  Each list sorted by order(x,y,z)

 */

void CSmatch3int(
  /* inputs */
  int *na,
  int *xa,
  int *ya,
  int *za, /* sorted into increasing order of (xa, ya, za) */
  int *nb,
  int *xb,
  int *yb,
  int *zb, /* sorted into increasing order of (xb, yb, zb) */
  /* output */
  int *match
  /* match[i] = j+1 if xb[j], yb[j], zb[j] matches xa[i], ya[i], za[i] */
  /* match[i] = 0   if no such point matches xa[i], ya[i], za[i] */
) { 
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
