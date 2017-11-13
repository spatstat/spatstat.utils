/*
    ply.h

    Template for functions in ply.c
    This file is #included several times

    Macros used:
    FNAME     Name of C routine
    NDIM      Number of dimensions of result (1, 2 or 3)

    Adrian Baddeley and Tilman Davies

    $Revision: 1.3 $  $Date: 2017/11/13 08:43:37 $

*/


void FNAME(nin, 
	   xin,  
	   iin,
#if (NDIM > 1)	   
	   jin,
#if (NDIM > 2)
	   kin,  
#endif
#endif	   
	   nout,
	   xout, 
	   iout	
#if (NDIM > 1)	      
	   , jout
#if (NDIM > 2)
	   , kout
#endif
#endif
) 
     int *nin, *nout;
     double *xin, *xout;
     int *iin, *iout;
#if (NDIM > 1)     
     int *jin, *jout;
#if (NDIM > 2)
     int *kin, *kout;
#endif
#endif
{
  int Nin, l, m, icur;
#if (NDIM > 1)  
  int jcur;
#if (NDIM > 2)
  int kcur;
#endif
#endif  
  double xsum;
  Nin = *nin;
  if(Nin == 0) {
    *nout = 0;
    return;
  }
  /* initialise first cell using first entry */
  m = 0;
  iout[0] = icur = iin[0];
#if (NDIM > 1)
  jout[0] = jcur = jin[0];
#if (NDIM > 2)
  kout[0] = kcur = kin[0];
#endif
#endif
  xout[0] = xsum = xin[0];
  /* process subsequent entries */
  if(Nin > 1) {
    for(l = 1; l < Nin; l++) {
      if(iin[l] == icur
#if (NDIM > 1)
	 && jin[l] == jcur 
#if (NDIM > 2)
	 && kin[l] == kcur
#endif
#endif
	 ) {
	/* increment current sum */
	xsum += xin[l];
      } else {
	/* write cell result */
	xout[m] = xsum;
	/* initialise next cell */
	++m;
	iout[m] = icur = iin[l];
#if (NDIM > 1)
	jout[m] = jcur = jin[l];
#if (NDIM > 2)
	kout[m] = kcur = kin[l];
#endif
#endif
	xsum = xin[l];
      }
      /* write last cell */
      xout[m] = xsum;
    }
  }
  *nout = m + 1;
}
