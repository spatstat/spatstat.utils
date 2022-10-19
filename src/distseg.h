/*
  distseg.h

  Distance to nearest line segment

  This is #included multiple times in distseg.c
  
  Macros used:

      FNAME       Function name

      WANT_INDEX  #defined if the output vector 'index' is required

  Author: Adrian Baddeley 2018-2021

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2021
  Licence: GNU Public Licence >= 2

  $Revision: 1.3 $ $Date: 2022/10/19 02:58:46 $

*/

void
FNAME(
     /* input */
     double	*xp,
     double     *yp,		        /* point/pixel coordinates */
     int	*npoints,
     double	*x0,
     double     *y0,
     double     *x1,
     double     *y1,	                /* line segment endpoints */
     int	*nsegments,
     double     *epsilon,               /* tolerance for short segments */
     /* output */
     double	*dist2		        /* squared distance from pixel 
					   to nearest line segment 
					   INITIALISED TO LARGE VALUE */
#ifdef WANT_INDEX
     , int	*index		        /* which line segment is closest */
#endif
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
	  if(dist2[i] > dsq) {
	    dist2[i] = dsq;
#ifdef WANT_INDEX	    
	    index[i] = j;
#endif	    
	  }
	}
      } else {
	/* short segment - use endpoints only */
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
	  if(dist2[i] > dsq) {
	    dist2[i] = dsq;
#ifdef WANT_INDEX	    
	    index[i] = j;
#endif	    
	  }
	}
      }
    }
  }
}

