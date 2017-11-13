/* 
   ply.c

   Faster versions of tapply(..., FUN=sum)
   assuming indices are sorted.

   Code template is in 'ply.h'

   Adrian Baddeley and Tilman Davies

   $Revision: 1.3 $ $Date: 2017/11/13 08:21:20 $

*/

#include <R.h>
#include <R_ext/Utils.h>
#include <Rmath.h>

#define FNAME ply3sum
#define NDIM 3
#include "ply.h"
#undef FNAME
#undef NDIM

#define FNAME ply2sum
#define NDIM 2
#include "ply.h"
#undef FNAME
#undef NDIM

#define FNAME ply1sum
#define NDIM 1
#include "ply.h"
#undef FNAME
#undef NDIM


