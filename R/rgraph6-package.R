#' rgraph6: Representing Graphs as graph6 Strings
#' 
#' This package implements methods for representing undirected graphs in a
#' compact 'graph6' format. Main functions are [as_graph6()], [as_dgraph6()], [as_sparse6()] and
#' [as_adjacency()]. The format is due to Brendan McKay
#' (\url{http://cs.anu.edu.au/~bdm}).
#' 
#' 
#' @section The graph6 format:
#' The description below is taken from \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}.
#' 
#' General principles: 
#' 
#' - All numbers in this description are in decimal unless obviously in binary.
#' - Apart from the header, there is one object per line.  Apart from the header
#' and the end-of-line characters, all bytes have a value in the range 63-126
#' (which are all printable ASCII characters).  A file of objects is a text
#' file, so whatever end-of-line convention is locally used is fine).
#'
#' 
#' Bit vectors:
#' 
#' A bit vector \eqn{x} of length \eqn{k} can be represented as follows.
#' Example: 1000101100011100
#' 
#' 1. Pad on the right with 0 to make the length a multiple of 6. Example: 100010110001110000
#' 2. Split into groups of 6 bits each.  Example: 100010 110001 110000
#' 3. Add 63 to each group, considering them as bigendian binary numbers. Example: 97 112 111
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
#' Now we can describe the actual file format:
#' 
#' Data type: simple undirected graphs of order 0 to 262143.
#' 
#' Optional Header: `>>graph6<<` (without end of line!)
#' 
#' File name extension: `.g6`
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
#' 
#' 
#' @section Description of dgraph6 format:
#' 
#' Data type: simple directed graphs (allowing loops) of order 0 to 68719476735.
#' 
#' Optional Header: \code{>>dgraph6<<} (without end of line!)
#' 
#' File name extension: \code{.d6}
#' 
#' One graph:
#' 
#' Suppose \eqn{G} has \eqn{n} vertices. Write the adjacency matrix of \eqn{G}
#' as a bit vector x of length \eqn{n^2}, row by row.
#'
#' Then the graph is represented as '&' \eqn{N(n) R(x)}.
#' The character '&' (decimal \eqn{38}) appears as the first character.
#' 
#' Example:
#' 
#' Suppose \eqn{n=5} and \eqn{G} has edges 0->2, 0->4, 3->1 and 3->4.
#' 
#' \deqn{x = 00101 00000 00000 01001 00000}
#' 
#' Then \eqn{N(n) = 68} and
#' \eqn{R(x) = R(00101 00000 00000 01001 00000) = 73  63  65  79  63}. So, the graph is  \eqn{38 68 73  63  65  79  63}.
#' 
#' 
#' @section Description of sparse6 format:
#' 
#' Data type: Undirected graphs of order 0 to 68719476735. Loops and multiple edges are permitted.
#' 
#' Optional Header: \code{>>sparse6<<} (without end of line!)
#' 
#' File name extension: \code{.s6}
#' 
#' General structure:
#' 
#' Each graph occupies one text line. Except for the first character
#' and end-of-line characters, each byte has the form \eqn{63+x}, where 
#' \eqn{0 <= x <= 63}. The byte encodes the six bits of x.
#'
#'The encoded graph consists of:  
#'
#'(1) The character ':'.   (This is present to distinguish the code from graph6 format.)
#'
#'(2) The number of vertices.
#'
#'(3) A list of edges.
#'
#'(4) end-of-line
#'
#'Loops and multiple edges are supported, but not directed edges.
#'
#' Number of vertices \eqn{n}: Same as graph6 format 
#' 
#' List of edges:
#' 
#' Let k be the number of bits needed to represent n-1 in binary.
#'
#'The remaining bytes encode a sequence 
#'\deqn{b[0] x[0] b[1] x[1] b[2] x[2] ... b[m] x[m]}
#' 
#' Each \eqn{b[i]} occupies 1 bit, and each \eqn{x[i]} occupies k bits.
#' Pack them together in bigendian order, and pad up to a multiple of 6 as follows:
#' 
#' 1. If \eqn{(n,k) = (2,1), (4,2), (8,3) or (16,4)}, and vertex
#'    \eqn{n-2} has an edge but \eqn{n-1} doesn't have an edge, and
#'     there are \eqn{k+1} or more bits to pad, then pad with one
#'     0-bit and enough 1-bits to complete the multiple of 6.
#'     
#' 2. Otherwise, pad with enough 1-bits to complete the
#'     multiple of 6.
#' 
#' Then represent this bit-stream 6 bits per byte as indicated above.
#' 
#' 
#' @references 
#' Brendan McKay \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}.
#' 
#' 
#' 
#' @docType package
#' @name rgraph6-package
#' @aliases rgraph6
#' @import Rcpp
#' @useDynLib rgraph6
NULL