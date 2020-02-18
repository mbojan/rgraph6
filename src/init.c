#include "decbin.h"
#include "R.h"
#include "Rinternals.h"
#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>

static R_NativePrimitiveArgType bin2dec_t[] = {
  INTSXP, INTSXP, INTSXP
};

static const R_CMethodDef cMethods[] = {
  {"bin2dec", (DL_FUNC) &bin2dec, 4, bin2dec_t},
  {NULL, NULL, 0, NULL}
};


void R_init_MyLib(DllInfo *info) {
  R_registerRoutines(info, cMethods, NULL, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}