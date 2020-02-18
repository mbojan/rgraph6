#include <stdio.h>
#include <stdlib.h>


// Binary to Decimal, Hexadecimal and Octal conversion program
// tested with Pelles C     vegaseat     15dec2004
void bin2dec( int *binary, int *decimal, int *len )
{
	int  b, k, m, n;
	int  sum = 0;

	for(k = 0; k < *len; k++) 
	{
		n = binary[k];
		for(b = 1, m = *len-1; m > k; m--) 
		{
			// appropriate power of 2
			b *= 2;
		}
		// sum it up
		sum = sum + n * b;
	}
	*decimal = sum;
}


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


