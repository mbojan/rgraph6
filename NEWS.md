# rgraph 2.0-4

- Fix use of `sample_gnp()` in the vignette. Thanks @szhorvat!  (#38)


# rgraph 2.0-3

- Fix deprecation of some of the coercion methods in **Matrix** package (#33).
- Update the vignette such that we don't expect `igraph::vcount()` to return an integer (#34, #35).

# rgraph6 2.0-2

- Re-roxygenize to fix HTML validation errors.


# rgraph6 2.0-1

- Fixed C-level errors (#31)


# rgraph6 2.0-0

This is a major overhaul of the package.

- The package now supports 'sparse6' and 'digraph6' formats.
- A more complete API with `graph_as_text()`, `igraph_from_text()`, `network_from_text()` top level functions and plethora low level functions.
- Added vignette.
- David Schoch joins as a coauthor.


# rgraph6 1.2

- Corrections in the documentation
- Added description of `graph6` format.


# rgraph6 1.1

- Corrected code for binary to decimal conversion. Previous version was returning wrong results for binary numbers that begin with 1 and have all other entries to 0.
- Added some tests for testing binary to decimal conversions as well as for converting matrices to graph6 format.


# rgraph6 1.0

- Added functions `b2d` and `d2b` for conversions between decimal and binary numbers. They are based on compiled C code so should be much faster than the older ones written in R.
- Functions `bin2dec` and `dec2bin` have been rewritten for use of newly added compiled code.
- Added a `sampleg6` file with couple of g6 symbols.


# rgraph6 0.0-1

- First beta version of the package.
