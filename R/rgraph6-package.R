#' @name rgraph6-package
#' @aliases rgraph6
#' @keywords package
#' @useDynLib rgraph6
#' @docType package
#' @import Rcpp
#' 
#' @details Formats 'graph6', 'sparse6' and 'digraph6' represent graphs as
#'   strings of printable ASCII characters. The formats are due to [Brendan
#'   McKay](http://cs.anu.edu.au/~bdm) and are described
#'   [here](http://cs.anu.edu.au/people/bdm/data/formats.txt).
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
#' @references 
#' McKay, B. D., & Piperno, A. (2014). Practical graph isomorphism, II. *Journal
#' of symbolic computation*, 60, 94-112.
#' 
"_PACKAGE"




# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
