# 1.2

- Corrections in the documentation
- Added description of `graph6` format.


# 1.1

- Corrected code for binary to decimal conversion. Previous version was returning wrong results for binary numbers that begin with 1 and have all other entries to 0.
- Added some tests for testing binary to decimal conversions as well as for converting matrices to graph6 format.

# 1.0

- Added functions `b2d` and `d2b` for conversions between decimal and binary numbers. They are based on compiled C code so should be much faster than the older ones written in R.
- Functions `bin2dec` and `dec2bin` have been rewritten for use of newly added compiled code.
- Added a \code{sampleg6} file with couple of g6 symbols.


# 0.0-1

- First beta version of the package.
