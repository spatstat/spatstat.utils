/*
  distseg.h

  Distance from point to nearest line segment

  This is #included multiple times in distseg.c
  
  Macros used:

      FNAME       Function name

      WANT_INDEX  #defined if the output vector 'index' is required

      WANT_PROJ   #defined if the projected point on the segment is required

  Author: Adrian Baddeley 2018-2026

  Copyright (C) Adrian Baddeley, Ege Rubak and Rolf Turner 2001-2026
  Licence: GNU Public Licence >= 2

  $Revision: 1.9 $ $Date: 2026/05/07 05:17:56 $

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
#ifdef WANT_PROJ
     , double   *xproj,                 /* coordinates of projected point */
     double *yproj,
     double *tproj                      /* local coordinate */
#endif     
) {
  int	i,j, np, nseg, maxchunk;
  double dx,dy,leng,co,si;  /* parameters of segment */
  double xpi, ypi, x0j, y0j, x1j, y1j; /* coordinates */
  double xdif0,ydif0,xdif1,ydif1,xrot,yrot; /* vectors */
  double dsq0,dsq1,dsq,dsqperp; /* squared distances */
  double eps;
  double *xpp, *ypp;        /* pointers into data xp[], yp[] */
  double *dist2p;           /* pointer into dist2[] */
#ifdef WANT_INDEX
  int    *indexp;           /* pointer into index[] */
#endif
#ifdef WANT_PROJ
  int isperp;
  double *xprojp, *yprojp;  /* pointers into xproj[], yproj[] */
  double *tprojp;           /* pointer into tproj[] */
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
	/* initialise pointers */
	xpp = xp;
	ypp = yp;
	dist2p = dist2;
#ifdef WANT_INDEX
	indexp = index;
#endif
#ifdef WANT_PROJ
	xprojp = xproj;
	yprojp = yproj;
	tprojp = tproj;
#endif	
	for(i = np; i > 0; i--) {
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
	  xrot = xdif0 * co + ydif0 * si;
#ifdef WANT_PROJ
	  isperp = 0;
#endif	  
	  /* perpendicular distance applies only in perpendicular region */
	  if(xrot >= 0.0 && xrot <= leng) {
	    yrot = -xdif0 * si + ydif0 * co;
	    dsqperp = yrot * yrot;
	    if(dsqperp < dsq) {
	      dsq = dsqperp;
#ifdef WANT_PROJ
	      isperp = 1;
#endif	  
	    }
	  }
	  if(*dist2p > dsq) {
	  /* update minimum distance for pixel i */
	    *dist2p = dsq;
#ifdef WANT_INDEX	    
	    *indexp = j;
#endif	    
#ifdef WANT_PROJ
	    if(isperp == 0) {
	      /* closest location was a segment endpoint */
	      if(dsq0 < dsq1) {
		*xprojp = x0j;
		*yprojp = y0j;
		*tprojp = 0.0;
	      } else {
		dsq = dsq1;
		*xprojp = x1j;
		*yprojp = y1j;
		*tprojp = 1.0;
	      }
	    } else {
	      /* closest location was an interior point on segment */
	      /* back-rotate the projected point (xrot, 0) */
	      *xprojp = x0j + xrot * co;
	      *yprojp = y0j + xrot * si;
	      /* fraction along segment */
	      *tprojp = xrot/leng;
	    }
#endif
	  }
	  /* increment pointers */
	  xpp++;
	  ypp++;
	  dist2p++;
#ifdef WANT_INDEX
	  indexp++;
#endif
#ifdef WANT_PROJ
	  xprojp++;
	  yprojp++;
	  tprojp++;
#endif
	}
      } else {
	/* short segment - use endpoints only */
	/* initialise pointers */
	xpp = xp;
	ypp = yp;
	dist2p = dist2;
#ifdef WANT_INDEX
	indexp = index;
#endif
#ifdef WANT_PROJ
	xprojp = xproj;
	yprojp = yproj;
	tprojp = tproj;
#endif	
	for(i = np; i > 0; i--) {
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
#ifdef WANT_PROJ	    
	    if(dsq0 < dsq1) {
	      *xprojp = x0j;
	      *yprojp = y0j;
	      *tprojp = 0.0;
	    } else {
	      dsq = dsq1;
	      *xprojp = x1j;
	      *yprojp = y1j;
	      *tprojp = 1.0;
	    }
#endif	      
	  }
	  /* increment pointers */
	  xpp++;
	  ypp++;
	  dist2p++;
#ifdef WANT_INDEX
	  indexp++;
#endif
#ifdef WANT_PROJ
	  xprojp++;
	  yprojp++;
	  tprojp++;
#endif	
	}
      }
    }
  }
}

