#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
double b2d(NumericVector x) {
  int len = x.size();
  int  b, k, m, n;
  int  total = 0;
  
  for(k = 0; k < len; k++) 
  {
    n = x[k];
    for(b = 1, m = len-1; m > k; m--) 
    {
      // appropriate power of 2
      b *= 2;
    }
    // sum it up
    total = total + n * b;
  }
  return total;
}

// [[Rcpp::export]]
NumericVector d2b(int x) {
  int  k = 0, n = 0;
  int  remain;
  int  len;
  if(x == 0) {
    len = 1;
  } else {
    len = floor( (log10(x)/log10(2)) + 1);
  }
  NumericVector binary(len);
  NumericVector temp(len);
  
  
  do 
  {
    remain    = x % 2;
    // whittle down the decimal number
    x   = x / 2;
    temp[k++] = remain;
  } while (x > 0);
  // reverse the order
  while (k >= 0)
    binary[n++] = temp[--k];
  return binary;
}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

/*** R
d2b(42)
b2d(c(1,0,1))
*/
