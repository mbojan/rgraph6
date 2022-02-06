#' @name rgraph6-package
#' @aliases rgraph6
#' @keywords package
#' @useDynLib rgraph6
#' @docType package
#' @import Rcpp
#' 
#' @details Formats 'graph6', 'sparse6' and 'digraph6' represent graphs as
#'   strings of printable ASCII characters. The formats are due to [Brendan
#'   McKay](http://cs.anu.edu.au/~bdm) who implemented them in his program
#'   `Nauty` (McKay 1978, 1980, 2003, McKay & Piperno 2014), and are described
#'   in detail [here](http://cs.anu.edu.au/people/bdm/data/formats.txt). Package
#'   \pkg{rgraph6} is a native R implementation of these formats.
#' 
#' The main functions are [as_graph6()], [as_digraph6()], [as_sparse6()] for
#' encoding network data and [igraph_from_text()] and [network_from_text()] for
#' decoding. There are also other low-level functions to decode directly from
#' [digraph6][from_digraph6], [graph6][from_graph6], and
#' [sparse6][from_sparse6].
#' 
#' @section Citation:
#' 
#' When using this package please cite it by referring to the following
#' publications:
#' 
#' ```{r, echo=FALSE, results="asis"}
#' print(readCitationFile("inst/CITATION"), style = "text")
#' ```
#' 
#' Call `citation(package="rgraph6")` for more details and the BibTeX entry.
#' 
#' @references 
#' McKay, B. D. (1978) Computing automorphisms and canonical labellings of
#' graphs *Combinatorial Mathematics, Lect. Notes Math.*, vol. 686,
#' Springer-Verlag, Berlin, pp. 223-232 \doi{10.1007/BFb0062536}
#' 
#' McKay, B. D. (1981). Practical graph isomorphism. *Congressus Numerantium*,
#' 30, pp. 45-87
#' 
#' McKay, B. D. (2003). *"Nauty" User’s Guide* (version 2.2) (p. 112). Technical
#' Report TR-CS-9002, Australian National University.
#' 
#' McKay, B. D., & Piperno, A. (2013). *Nauty and Traces user’s guide* (Version
#' 2.5). Computer Science Department, Australian National University, Canberra,
#' Australia.
#' 
#' McKay, B. D., & Piperno, A. (2014). Practical graph isomorphism, II. *Journal
#' of symbolic computation*, 60, 94-112. \doi{10.1016/j.jsc.2013.09.003}
#' 
"_PACKAGE"




# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
