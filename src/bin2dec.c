// Binary to Decimal, Hexadecimal and Octal conversion program
// tested with Pelles C     vegaseat     15dec2004

#include <stdio.h>


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

