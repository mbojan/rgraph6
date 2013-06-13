\name{graph6}
\alias{graph6}
\title{Description of the graph6 format}
\description{
Description of graph6 format for storing undirected graphs.
}
\details{
General principles:
\itemize{
  \item All numbers in this description are in decimal unless obviously in
  binary.

  \item Apart from the header, there is one object per line.  Apart from the
  header and the end-of-line characters, all bytes have a value in the range
  63-126 (which are all printable ASCII characters).  A file of objects is a
  text file, so whatever end-of-line convention is locally used is fine).
}

Bit vectors:

  A bit vector \eqn{x} of length \eqn{k} can be represented as follows.  
      Example:  1000101100011100

\enumerate{
  \item Pad on the right with 0 to make the length a multiple of 6.
      Example:  100010110001110000

  \item Split into groups of 6 bits each.
      Example:  100010 110001 110000

  \item Add 63 to each group, considering them as bigendian binary numbers.
      Example:  97 112 111
}

These values are then stored one per byte. So, the number of bytes is
\eqn{ceiling(k/6)}.

Let \eqn{R(x)} denote this representation of x as a string of bytes.
      
Small nonnegative integers:
 
Let \eqn{n} be an integer in the range 0-262143 (\eqn{262143 = 2^18-1}).

If \eqn{0 \leq n \leq 62}{0 <= n <= 62}, define \eqn{N(n)} to be the single
byte \eqn{n+63}.  If \eqn{n \geq 63}{n >= 63}, define \eqn{N(n)} to be the four
bytes \eqn{126 R(x)}, where \eqn{x} is the bigendian 18-bit binary form of
\eqn{n}.

Examples:  
\deqn{N(30) = 93}
\deqn{N(12345) = N(000011 000000 111001) = 126 69 63 120}


\section{Description of graph6 format}


Data type:  simple undirected graphs of order 0 to 262143.

Optional Header: \code{>>graph6<<}     (without end of line!)

File name extension: \code{.g6}

One graph:

Suppose \eqn{G} has \eqn{n} vertices.  Write the upper triangle of the
adjacency matrix of \eqn{G} as a bit vector \eqn{x} of length \eqn{n(n-1)/2},
using the ordering \eqn{(0,1),(0,2),(1,2),(0,3),(1,3),(2,3),...,(n-1,n)}.

Then the graph is represented as  N(n) R(x).

Example:

Suppose \eqn{n=5} and \eqn{G} has edges 0-2, 0-4, 1-3 and 3-4.

\deqn{x = 0 10 010 1001}

Then \eqn{N(n) = 68} and \eqn{R(x) = R(010010 100100) = 81 99}.
So, the graph is  \eqn{68 81 99}.
}
\references{\url{http://cs.anu.edu.au/people/bdm/data/formats.txt}}
\author{Michal Bojanowski \email{mbojan@ifispan.waw.pl} based on the above webpage}
\seealso{\code{\link{asAMatrix}} and \code{\link{asGraph6}} for
conversion functions.}
\keyword{manip}
