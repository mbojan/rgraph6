#include "decbin.h"
#include "R.h"
#include "Rinternals.h"
#include <R_ext/Rdynload.h>


#define CDEF(name) {#name, (DL_FUNC) &name, sizeof(name ## _t)/sizeof(name ## _t[0]), name ##_t}


static R_NativePrimitiveArgType bin2dec_t[] = {
  INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType dec2bin_t[] = {
  INTSXP, CHARSXP, INTSXP
};





static const R_CMethodDef CEntries[] = {
  CDEF(bin2dec),
  CDEF(dec2bin),
  {NULL, NULL, 0}
};



void R_init_rgraph6(DllInfo *info) {
  R_registerRoutines(info, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
