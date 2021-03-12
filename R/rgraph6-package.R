#' rgraph6: Representing Graphs as 'graph6', 'digraph6' or 'sparse6' Strings
#' 
#' This package implements methods for representing graphs in formats 'graph6',
#' 'sparse6' and 'digraph6' as strings of printable ASCII characters. The
#' formats are due to [Brendan McKay](http://cs.anu.edu.au/~bdm) and are
#' described [here](http://cs.anu.edu.au/people/bdm/data/formats.txt).
#' 
#' The main functions are [as_graph6()], [as_digraph6()], [as_sparse6()] for
#' encoding network data and [igraph_from_text()] and [network_from_text()] for
#' decoding. There are also other low-level functions to decode directly from
#' [digraph6][from_digraph6], [graph6][from_graph6], and
#' [sparse6][from_sparse6].
#' 
#' @section Citation:
#' 
#' The formats themselves are by [Brendan McKay](http://cs.anu.edu.au/~bdm).
#' 
#' When using this package please cite it by referring to:
#' 
#' 
#' ```{r, echo=FALSE, results="asis"}
#' print(readCitationFile("inst/CITATION"), style = "text")
#' ```
#' 
#' Call `citation(package="rgraph6")` for more details and the BibTeX entry.
#' 
#' @docType package
#' @name rgraph6-package
#' @aliases rgraph6
#' @import Rcpp
#' @useDynLib rgraph6
"_PACKAGE"