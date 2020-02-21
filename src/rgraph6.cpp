#include <Rcpp.h>
using namespace Rcpp;

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
std::vector<double> d2b(unsigned long int x){
  int  k = 0;
  int  remainder;
  int  len;
  if(x == 0) {
    len = 1;
  } else {
    len = (int) floor( (log10(x)/log10(2)) + 1);
  }
  std::vector<double> binary(len);

  while (x!=0)
  {
    remainder = x%2;
    x /= 2;
    binary[k] = remainder;
    k+=1;
  }

  std::reverse(binary.begin(), binary.end()); 
  return binary;
}