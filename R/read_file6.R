#' Read files graph6, sparse6 or digraph6 symbols
#' 
#' @param file character; path to file name
#' @param type character; one of "adjacency", "edgelist", "igraph", or
#'   "network". Type of result returned.
#' 
#' @return A list of decoded graphs in the form of objects determined by `type`.

read_file6 <- function(file, type="adjacency"){
  type <- match.arg(type,c("adjacency","edgelist","igraph","network"))
  txt <- readLines(file)
  switch(type,
         "adjacency" = adjacency_from_text(txt),
         "edgelist" = edgelist_from_text(txt),
         "igraph"   = igraph_from_text(txt),
         "network"  = network_from_text(txt)
  )
}