#include "decbin.h"
#include "R.h"
#include "Rinternals.h"
#include <R_ext/Rdynload.h>


#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

/*
static R_NativePrimitiveArgType bin2dec_t[] = {
  INTSXP, INTSXP, INTSXP
};

static R_NativePrimitiveArgType dec2bin_t[] = {
  INTSXP, CHARSXP, INTSXP
};
*/




static const R_CallMethodDef CallEntries[] = {
  CALLDEF(bin2dec, 3),
  CALLDEF(dec2bin, 3),
  {NULL, NULL, 0}
};



void R_init_rgraph6(DllInfo *info) {
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
