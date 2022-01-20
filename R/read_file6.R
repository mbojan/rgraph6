#' Read files of 'graph6', 'sparse6' or 'digraph6' symbols
#' 
#' @param path character; path to file name
#' @param type character; one of "adjacency", "edgelist", "igraph", or
#'   "network". Type of result returned.
#'   
#' @details File pointed to by `path` is a text file with one graph symbol per line.
#'   Optional headers of the form `>>graph6<<` or `>>sparse6<<` in the first
#'   line (and without the newline after the header) are ignored and removed.
#' 
#' @return A list of decoded graphs in the form of objects determined by `type`.
#' @examples 
#' g6_file <- tempfile()
#' write(sampleg6,g6_file)
#' read_file6(g6_file, type = "adjacency")
#' unlink(g6_file)
#' @export

read_file6 <- function(path, type="adjacency"){
  type <- match.arg(type, c("adjacency", "edgelist", "igraph", "network"))
  txt <- readLines(path)
  txt[1] <- gsub("^>>[^<]+<<", "", txt[1]) # Remove potential header
  txt <- txt[txt != ""] # Remove empty lines
  switch(
    type,
    "adjacency" = adjacency_from_text(txt),
    "edgelist" = edgelist_from_text(txt),
    "igraph"   = igraph_from_text(txt),
    "network"  = network_from_text(txt)
  )
}
