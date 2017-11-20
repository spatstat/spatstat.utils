
/* 
   Native symbol registration table for spatstat.utils package

   Automatically generated - do not edit this file!

*/

#include "proto.h"
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*  
   See proto.h for declarations for the native routines registered below.
*/

static const R_CMethodDef CEntries[] = {
    {"Cmatchxy",    (DL_FUNC) &Cmatchxy,     7},
    {"CSmatch2int", (DL_FUNC) &CSmatch2int,  7},
    {"CSmatch3int", (DL_FUNC) &CSmatch3int,  9},
    {"CUmatch2int", (DL_FUNC) &CUmatch2int,  7},
    {"CUmatch3int", (DL_FUNC) &CUmatch3int,  9},
    {"drevcumsum",  (DL_FUNC) &drevcumsum,   2},
    {"fastinterv",  (DL_FUNC) &fastinterv,   5},
    {"inxyp",       (DL_FUNC) &inxyp,        8},
    {"irevcumsum",  (DL_FUNC) &irevcumsum,   2},
    {"nndist2segs", (DL_FUNC) &nndist2segs, 11},
    {"ply1sum",     (DL_FUNC) &ply1sum,      6},
    {"ply2sum",     (DL_FUNC) &ply2sum,      8},
    {"ply3sum",     (DL_FUNC) &ply3sum,     10},
    {"prdist2segs", (DL_FUNC) &prdist2segs, 10},
    {"primefax",    (DL_FUNC) &primefax,     3},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"circMseg", (DL_FUNC) &circMseg, 7},
    {"circXseg", (DL_FUNC) &circXseg, 7},
    {NULL, NULL, 0}
};

void R_init_spatstat_utils(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
