/*
  distseg.h

  Distance to nearest line segment

  This is #included multiple times in distseg.c
  
  Macros used:

      FNAME       Function name

      WANT_INDEX  #defined if the output vector 'index' is required

  Author: Adrian Baddeley 2018-2021

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2026
  Licence: GNU Public Licence >= 2

  $Revision: 1.5 $ $Date: 2026/05/02 07:03:14 $

*/

void
FNAME(
     /* input */
     double	*xp,
     double     *yp,		        /* point (or pixel) coordinates */
     int	*npoints,
     double	*x0,
     double     *y0,
     double     *x1,
     double     *y1,	                /* line segment endpoints */
     int	*nsegments,
     double     *epsilon,               /* tolerance for short segments */
     /* output */
     double	*dist2		        /* squared distance from point
					   to nearest line segment 
					   INITIALISED TO LARGE VALUE */
#ifdef WANT_INDEX
     , int	*index		        /* which line segment is closest */
#endif
) {
  int	i,j, np, nseg, maxchunk;
  double dx,dy,leng,co,si;  /* parameters of segment */
  double xpi, ypi, x0j, y0j, x1j, y1j; /* coordinates */
  double xdif0,ydif0,xdif1,ydif1,xpr,ypr; /* vectors */
  double dsq0,dsq1,dsq,dsqperp; /* squared distances */
  double eps;
  double *xpp, *ypp;        /* pointers into data xp[], yp[] */
  double *dist2p;           /* pointer into dist2[] */
#ifdef WANT_INDEX
  int    *indexp;           /* pointer into index[] */
#endif

  np   = *npoints;
  nseg = *nsegments;
  eps  = *epsilon;

  OUTERCHUNKLOOP(j, nseg, maxchunk, 16384) {
    R_CheckUserInterrupt();
    INNERCHUNKLOOP(j, nseg, maxchunk, 16384) {
      /* segment j coordinates */
      x0j = x0[j];
      y0j = y0[j];
      x1j = x1[j];
      y1j = y1[j];
      /* segment j geometry */
      dx = x1j - x0j;
      dy = y1j - y0j;
      leng = hypot(dx, dy);
      /* loop over points */
      if(leng > eps) {
	/* normal case */
	co = dx/leng;
	si = dy/leng;
	for(
#ifdef WANT_INDEX
	    i = np, xpp = xp, ypp = yp, dist2p = dist2, indexp = index;
	    i > 0;
	    i--,    xpp++,    ypp++,    dist2p++,       indexp++
#else	    
	    i = np, xpp = xp, ypp = yp, dist2p = dist2;
	    i > 0;
	    i--,    xpp++,    ypp++,    dist2p++
#endif	    
	    ) {
	  /* point i coordinates */
	  xpi = *xpp;
	  ypi = *ypp;
	  /* vectors from point i to endpoints of segment j */
	  xdif0 =  xpi - x0j;
	  ydif0 =  ypi - y0j;
	  xdif1 =  xpi - x1j;
	  ydif1 =  ypi - y1j;
	  /* squared distances to segment endpoints */
	  dsq0 = xdif0 * xdif0 + ydif0 * ydif0;
	  dsq1 = xdif1 * xdif1 + ydif1 * ydif1;
	  dsq = (dsq0 < dsq1) ? dsq0 : dsq1;
	  /* rotate pixel around 1st endpoint of segment
	     so that line segment lies in x axis */
	  xpr = xdif0 * co + ydif0 * si;
	  /* perpendicular distance applies only in perpendicular region */
	  if(xpr >= 0.0 && xpr <= leng) {
	    ypr = -xdif0 * si + ydif0 * co;
	    dsqperp = ypr * ypr;
	    if(dsqperp < dsq) dsq = dsqperp;
	  }
	  /* update minimum distance for pixel i */
	  if(*dist2p > dsq) {
	    *dist2p = dsq;
#ifdef WANT_INDEX	    
	    *indexp = j;
#endif	    
	  }
	}
      } else {
	/* short segment - use endpoints only */
	for(
#ifdef WANT_INDEX
	    i = np, xpp = xp, ypp = yp, dist2p = dist2, indexp = index;
	    i > 0;
	    i--,    xpp++,    ypp++,    dist2p++,       indexp++
#else	    
	    i = np, xpp = xp, ypp = yp, dist2p = dist2;
	    i > 0;
	    i--,    xpp++,    ypp++,    dist2p++
#endif	    
	    ) {
	  /* point i coordinates */
	  xpi = *xpp;
	  ypi = *ypp;
	  /* vectors from point i to endpoints of segment j */
	  xdif0 =  xpi - x0j;
	  ydif0 =  ypi - y0j;
	  xdif1 =  xpi - x1j;
	  ydif1 =  ypi - y1j;
	  /* squared distances to segment endpoints */
	  dsq0 = xdif0 * xdif0 + ydif0 * ydif0;
	  dsq1 = xdif1 * xdif1 + ydif1 * ydif1;
	  dsq = (dsq0 < dsq1) ? dsq0 : dsq1;
	  /* update minimum distance for pixel i */
	  if(*dist2p > dsq) {
	    *dist2p = dsq;
#ifdef WANT_INDEX	    
	    *indexp = j;
#endif	    
	  }
	}
      }
    }
  }
}

