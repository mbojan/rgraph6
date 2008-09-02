#include <stdio.h>
#include <stdlib.h>

void dec2bin(int *decimal, char *binary, int *len)
{
  int  k = 0, n = 0;
  int  remain;
  char *temp;

  temp = malloc(*len * sizeof(char));

  do 
  {
    remain    = *decimal % 2;
    // whittle down the decimal number
    *decimal   = *decimal / 2;
    // converts digit 0 or 1 to character '0' or '1'
    temp[k++] = remain + '0';
  } while (*decimal > 0);
  // reverse the order
  while (k >= 0)
    binary[n++] = temp[--k];
  free(temp);
}


