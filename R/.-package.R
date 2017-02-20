

#' Convert a graph6 symbol to adjacency matrix
#' 
#' This function takes a graph6 symbol and converts it to a symmetric square
#' adjacency matrix.
#' 
#' For detailed explanations of graph6 format see \code{\link{graph6}}.
#' 
#' @aliases asAMatrix asAMatrix-methods asAMatrix,character-method
#' @param object character, a graph6 symbol
#' @return A square symmetric adjacency matrix representing the graph.
#' @note This function aim at implementing format and methods originally
#' created by Brendan McKay, \email{bdm@cs.anu.edu.au} and
#' \url{http://cs.anu.edu.au/~bdm}.
#' @author Michal Bojanowski \email{mbojan@@ifispan.waw.pl}
#' @seealso \code{\link{graph6}} for format description and
#' \code{\link{asGraph6}} for conversion in the opposite direction.
#' @references \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}
#' @keywords manip methods
#' @examples
#' 
#' data(sampleg6)
#' sampleg6
#' l <- lapply(sampleg6, asAMatrix)
#' l
#' 
NULL





#' Convert adjacency matrix to a graph6 symbol
#' 
#' This function converts a graph adjacency matrix to a graph6 symbol.
#' 
#' If necessary, more details than the description above
#' 
#' @aliases asGraph6 asGraph6-methods asGraph6,matrix-method
#' @param object a square adjacency matrix containing 0s and 1s
#' @return A graph6 symbol.
#' @note This function aim at implementing format and methods originally
#' created by Brendan McKay, \email{bdm@cs.anu.edu.au} and
#' \url{http://cs.anu.edu.au/~bdm}.
#' @author Michal Bojanowski \email{mbojan@@ifispan.waw.pl}
#' @seealso \code{\link{graph6}} for format description and
#' \code{\link{asAMatrix}} for conversion in the opposite direction.
#' @references \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}
#' @keywords manip methods
#' @examples
#' 
#' g <- asAMatrix("F?Dco")
#' g
#' asGraph6(g)
#' 
NULL





#' Converting between decimal and binary numbers
#' 
#' These functions provide user-friendly interface to low-level routines
#' \code{b2d} and \code{d2b} for converting between decimal and binary numbers.
#' 
#' 
#' @aliases bin2dec dec2bin bin2dec-methods bin2dec,numeric-method
#' bin2dec,character-method
#' @param bin binary number as a numeric vector of 0s and 1s or a single
#' character string containing 0s and 1s only
#' @param dec single decimal number to be converted
#' @param num logical, whether a numeric vector (instead of single character
#' string) should be returned
#' @return For \code{bin2dec} the decimal number representing the binary input.
#' 
#' For \code{dec2bin}, if \code{num} is \code{TRUE}, which is the default, the
#' numeric vector containing the binary number. If \code{num} is \code{FALSE}
#' then a single string containing the sequence of 0s and 1s is returned.
#' @seealso \code{\link{b2d}} and \code{\link{d2b}} for low-level procedures
#' @keywords math
#' @examples
#' 
#' bin2dec("1101")
#' bin2dec(c(1,1,0,1))
#' 
#' dec2bin(13)
#' dec2bin(13, FALSE)
#' 
NULL





#' Description of the graph6 format
#' 
#' Description of graph6 format for storing undirected graphs.
#' 
#' General principles: \itemize{ \item All numbers in this description are in
#' decimal unless obviously in binary.
#' 
#' \item Apart from the header, there is one object per line.  Apart from the
#' header and the end-of-line characters, all bytes have a value in the range
#' 63-126 (which are all printable ASCII characters).  A file of objects is a
#' text file, so whatever end-of-line convention is locally used is fine). }
#' 
#' Bit vectors:
#' 
#' A bit vector \eqn{x} of length \eqn{k} can be represented as follows.
#' Example: 1000101100011100
#' 
#' \enumerate{ \item Pad on the right with 0 to make the length a multiple of
#' 6.  Example: 100010110001110000
#' 
#' \item Split into groups of 6 bits each.  Example: 100010 110001 110000
#' 
#' \item Add 63 to each group, considering them as bigendian binary numbers.
#' Example: 97 112 111 }
#' 
#' These values are then stored one per byte. So, the number of bytes is
#' \eqn{ceiling(k/6)}.
#' 
#' Let \eqn{R(x)} denote this representation of x as a string of bytes.
#' 
#' Small nonnegative integers:
#' 
#' Let \eqn{n} be an integer in the range 0-262143 (\eqn{262143 = 2^18-1}).
#' 
#' If \eqn{0 \leq n \leq 62}{0 <= n <= 62}, define \eqn{N(n)} to be the single
#' byte \eqn{n+63}.  If \eqn{n \geq 63}{n >= 63}, define \eqn{N(n)} to be the
#' four bytes \eqn{126 R(x)}, where \eqn{x} is the bigendian 18-bit binary form
#' of \eqn{n}.
#' 
#' Examples: \deqn{N(30) = 93} \deqn{N(12345) = N(000011 000000 111001) = 126
#' 69 63 120}
#' 
#' @section Description of graph6 format:
#' 
#' Data type: simple undirected graphs of order 0 to 262143.
#' 
#' Optional Header: \code{>>graph6<<} (without end of line!)
#' 
#' File name extension: \code{.g6}
#' 
#' One graph:
#' 
#' Suppose \eqn{G} has \eqn{n} vertices.  Write the upper triangle of the
#' adjacency matrix of \eqn{G} as a bit vector \eqn{x} of length
#' \eqn{n(n-1)/2}, using the ordering
#' \eqn{(0,1),(0,2),(1,2),(0,3),(1,3),(2,3),...,(n-1,n)}.
#' 
#' Then the graph is represented as N(n) R(x).
#' 
#' Example:
#' 
#' Suppose \eqn{n=5} and \eqn{G} has edges 0-2, 0-4, 1-3 and 3-4.
#' 
#' \deqn{x = 0 10 010 1001}
#' 
#' Then \eqn{N(n) = 68} and \eqn{R(x) = R(010010 100100) = 81 99}. So, the
#' graph is \eqn{68 81 99}.
#' @author Michal Bojanowski based on the above webpage
#' @seealso \code{\link{asAMatrix}} and \code{\link{asGraph6}} for conversion
#' functions.
#' @references \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}
#' @keywords manip
NULL





#' Interface to graph6 format for R
#' 
#' This package contain implementation of methods that allow to represent
#' undirected graphs in a compact 'graph6' format
#' 
#' This package implements routines for reading and writing undirected graphs
#' in graph6 format. The format itself was created by Brendan McKay
#' \url{http://cs.anu.edu.au/~bdm}. See \code{help(graph6)} for detailed format
#' description. The routines in this package are inspired by routines in
#' McKay's \code{nauty} suite of programs for graph analysis.
#' 
#' See \code{help(package="rgraph6")} for a list of available functions.
#' 
#' The main interface consists of two functions: \code{\link{asGraph6}} and
#' \code{\link{asAMatrix}}. The first one is for converting symmetric, square
#' binary matrices representing an undirected graphs to a graph6 symbol. The
#' second one for converting graph6 symbol into a square, symmetric binary
#' matrix (adjacency matrix).
#' 
#' @name rgraph6-package
#' @aliases rgraph6-package rgraph6
#' @docType package
#' @section Changes:
#' 
#' \bold{Version 1.2} (2007-11-19) \itemize{ \item Corrections in the
#' documentation.
#' 
#' \item Added description of the \code{graph6} format on \code{help(graph6)}
#' page }
#' 
#' \bold{Version 1.1} (2007-06-12)
#' 
#' \itemize{ \item Corrected code for binary to decimal conversion. Previous
#' version was returning wrong results for binary numbers that begin with 1 and
#' have all other entries to 0.
#' 
#' \item Added some tests for testing binary to decimal conversions as well as
#' for converting matrices to graph6 format. }
#' 
#' \bold{Version 1.0} (2007-06-06)
#' 
#' \itemize{ \item Added functions \code{\link{b2d}} and \code{\link{d2b}} for
#' conversions between decimal and binary numbers. They are based on compiled C
#' code so should be much faster than the older ones written in .
#' 
#' \item Functions \code{\link{bin2dec}} and \code{\link{dec2bin}} have been
#' rewritten for use of newly added compiled code.
#' 
#' \item Added a \code{sampleg6} file with couple of g6 symbols }
#' 
#' \bold{Version 0.0-1} (2007-02-09)
#' 
#' \itemize{ \item First beta version of the package }
#' @author Michal Bojanowski \email{mbojan@@ifispan.waw.pl}
#' @references The web page of Brendan McKay: \url{http://cs.anu.edu.au/~bdm}
#' @keywords package math
#' @examples
#' 
#' # TODO add examples
#' 
NULL





#' A sample vector of g6 codes
#' 
#' A vector of graph6 symbols each representing an undirected graph.
#' 
#' 
#' @name sampleg6
#' @docType data
#' @format The format is: chr [1:9] "CR" "CJ" "CN" ...
#' @seealso See \code{\link{graph6}} for format description,
#' \code{\link{asAMatrix}} and \code{\link{asGraph6}} for conversion routines.
#' @keywords datasets
#' @examples
#' 
#' data(sampleg6)
#' l <- lapply( sampleg6, asAMatrix )
#' # list of adjacency matrices
#' str(l)
#' 
NULL



