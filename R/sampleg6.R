#' Example vectors of 'graph6', 'sparse6', and 'digraph6' codes
#' 
#' Objects `g6`, `s6`, and `d6` are vectors of codes in 'graph6', 'sparse6', and
#' 'digraph6' representations respectively. Object `sampleg6` is a vector of
#' 'graph6' codes.
#' 
#' @usage g6
#' @usage s6
#' @usage d6
#' @usage sampleg6
#' 
#' @format The three objects `g6`, `s6`, and `d6` are character vectors of
#'   length 20 corresponding to undirected (in case of `g6` and `s6`) and
#'   directed (in case of `d6`) graphs of varying sizes and densities.
#'   
#'   Object `sampleg6` is a character vector of length `r length(sampleg6)` of
#'   undirected graphs in 'graph6' format.
#' 
#' @details Graphs in `g6`, `s6`, and `d6` objects were generated using the
#'   common algorithm which consists of the following steps:
#'   
#'   1. For each value from the vector of sizes of the node set (15, 30, 60, 120)...
#'   2. ... generate a vector of edge counts (size of the edge set) of length 5
#'   ranging from a single edge up to an edge count corresponding to the density
#'   of 0.2.
#'   3. Given the node set sizes (item 1) and edge set sizes (item 2) sample
#'   undirected graphs from GNM model.
#'   4. These undirected graphs are encoded in `g6` and `s6`
#'   5. Directed graphs were created by turning undirected edges to directed
#'   arcs in an arbitrary manner. These are encoded in the `d6` object.
#' 
#' 
#' @docType data
#' @aliases g6 d6 s6
"sampleg6"

