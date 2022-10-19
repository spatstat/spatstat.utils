/*
       distseg.c

       Distances from point pattern to line segment pattern
       Distance transform of a line segment pattern

       nnd2segs:    minimum distance from point to any line segment
       nndist2segs: minimum distance from point to any line segment (with index)
       prdist2segs: pairwise distances from each point to each line segment

       $Revision: 1.12 $ $Date: 2022/10/19 02:56:36 $

       Author: Adrian Baddeley

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2018
  Licence: GNU Public Licence >= 2

*/

#include <R.h>
#include <Rmath.h>
#include <R_ext/Utils.h>
#include <math.h>

#include "chunkloop.h"

#define FNAME nndist2segs
#define WANT_INDEX
#include "distseg.h"
#undef WANT_INDEX
#undef FNAME

#define FNAME nnd2segs
#include "distseg.h"
#undef FNAME


void
prdist2segs(
     /* input */
     double	*xp,
     double     *yp,		        /* point/pixel coordinates */
     int	*npoints,
     double	*x0,
     double     *y0,
     double     *x1,
     double     *y1, 	                /* line segment endpoints */
     int	*nsegments,
     double     *epsilon,               /* tolerance for short segments */
     /* output */
     double	*dist2		        /* squared distances from each pixel 
                                        to each line segment */
) {
  int	i,j, np, nseg, maxchunk;
  double dx,dy,leng,co,si;  /* parameters of segment */
  double xdif0,ydif0,xdif1,ydif1,xpr,ypr; /* vectors */
  double dsq0,dsq1,dsq,dsqperp; /* squared distances */
  double eps;

  np   = *npoints;
  nseg = *nsegments;
  eps  = *epsilon;

  OUTERCHUNKLOOP(j, nseg, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(j, nseg, maxchunk, 16384) {
      dx = x1[j] - x0[j];
      dy = y1[j] - y0[j];
      leng = hypot(dx, dy);
      if(leng > eps) {
	/* normal case */
	co = dx/leng;
	si = dy/leng;
	for(i = 0; i < np; i++) {
	  /* vectors from pixel to segment endpoints */
	  xdif0 =  xp[i] - x0[j];
	  ydif0 =  yp[i] - y0[j];
	  xdif1 =  xp[i] - x1[j];
	  ydif1 =  yp[i] - y1[j];
	  /* squared distances to segment endpoints */
	  dsq0 = xdif0 * xdif0 + ydif0 * ydif0;
	  dsq1 = xdif1 * xdif1 + ydif1 * ydif1;
	  dsq = (dsq0 < dsq1) ? dsq0 : dsq1;
	  /* rotate pixel around 1st endpoint of segment
	     so that line segment lies in x axis */
	  xpr = xdif0 * co + ydif0 * si;
	  ypr = -xdif0 * si + ydif0 * co;
	  /* perpendicular distance applies only in perpendicular region */
	  if(xpr >= 0.0 && xpr <= leng) {
	    dsqperp = ypr * ypr;
	    if(dsqperp < dsq) dsq = dsqperp;
	  }
	  dist2[i + j * np] = dsq;
	}
      } else {
	/* short segment */
	for(i = 0; i < np; i++) {
	  /* vectors from pixel to segment endpoints */
	  xdif0 =  xp[i] - x0[j];
	  ydif0 =  yp[i] - y0[j];
	  xdif1 =  xp[i] - x1[j];
	  ydif1 =  yp[i] - y1[j];
	  /* squared distances to segment endpoints */
	  dsq0 = xdif0 * xdif0 + ydif0 * ydif0;
	  dsq1 = xdif1 * xdif1 + ydif1 * ydif1;
	  dsq = (dsq0 < dsq1) ? dsq0 : dsq1;
	  dist2[i + j * np] = dsq;
	}
      }
    }
  }	
}

