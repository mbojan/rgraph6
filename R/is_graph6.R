#' Infer or test for graph6, sparse6, and dgraph6 symbols
#' 
#' Functions [is_graph6()], [is_sparse6()]), and [is_dgraph6()] test if elements
#' of a character vector are valid symbols of particular type.
#' 
#' @param x character vector
#' 
#' @return Logical vector of length equal to `length(x)` with `TRUE` if an
#'   element is a valid symbol and `FALSE` otherwise.
#' 
#' @note At this moment the test is performed using regular expressions.
#'   Theoretically it may result in false positives.
#' 
#' @export
#' 
#' @examples 
#' all(is_graph6(g6))
is_graph6 <- function(x) {
  grepl("^[][}{?@\\^_`|~a-zA-Z]+", x) &
    grepl("^[^:]", x) &
    grepl("^[^&]", x)
}


#' @rdname is_graph6
#' @export
#' 
#' @examples 
#' all(is_sparse6(s6))
is_sparse6 <- function(x) {
  grepl(":[][}{?@\\^_`|~a-zA-Z]+", x)
}

#' @rdname is_graph6
#' @export
#' 
#' @examples 
#' all(is_dgraph6(d6))
is_dgraph6 <- function(x) {
  grepl("&[][}{?@\\^_`|~a-zA-Z]+", x)
}



#' @rdname is_graph6
#' 
#' @description Function [guess_format()] tries to guess the type of the symbols
#' used in `x`.
#' 
#' @return Function [guess_format()] returns a character vector of the same
#'   length as `x` with values "graph6", "sparse6", or "dgraph6" depending on
#'   the type of symbol present, or `NA` if the symbol is unknown or matches
#'   more than one type.
#' 
#' @export
#' 
#' @examples 
#' 
#' # Vector mixing graphs in various formats
#' x <- g6
#' x[seq(2, 20, by = 3)] <- s6[seq(2, 20, by = 3)]
#' x[seq(3, 20, by = 3)] <- d6[seq(3, 20, by = 3)]
#' guess_format(x)
guess_format <- function(x) {
  mat <- cbind(g6 = is_graph6(x), s6 = is_sparse6(x), d6 = is_dgraph6(x))
  s <- apply(mat, 1, sum)
  res <- character(length(x))
  equiv <- s > 1
  if(any(equiv)) {
    warning("the following symbols (", sum(equiv), ") match more than one type: ",
            paste(x[equiv], collapse=", "))
    res[equiv] <- NA
  }
  unk <- s == 0
  if(any(unk)) {
    warning("the following symbols (", sum(unk), ") do not match any type: ",
            paste(x[unk], collapse=", "))
    res[unk] <- NA
  }
  res[!equiv & !unk] <- colnames(mat)[apply(mat[!equiv & !unk, , drop = FALSE], 1, which)]
  res
}
