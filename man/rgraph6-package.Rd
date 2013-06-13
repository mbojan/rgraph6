\name{rgraph6-package}
\alias{rgraph6-package}
\alias{rgraph6}
\docType{package}
\title{Interface to graph6 format for R}
\description{
This package contain implementation of methods that allow
to represent undirected graphs in a compact 'graph6' format
}
\details{
This package implements routines for reading and writing undirected graphs in
graph6 format. The format itself was created by Brendan McKay
\url{http://cs.anu.edu.au/~bdm}. See \code{help(graph6)} for detailed format
description. The routines in this package are inspired by routines in McKay's
\code{nauty} suite of programs for graph analysis.

See \code{help(package="rgraph6")} for a list of available functions.

The main interface consists of two functions: \code{\link{asGraph6}} and
\code{\link{asAMatrix}}. The first one is for converting symmetric, square
binary matrices representing an undirected graphs to a graph6 symbol. The
second one for converting graph6 symbol into a square, symmetric binary matrix
(adjacency matrix).
}
\author{
Michal Bojanowski \email{mbojan@ifispan.waw.pl}
}
\references{
The web page of Brendan McKay: \url{http://cs.anu.edu.au/~bdm}
}

\section{Changes}{

\bold{Version 1.2} (2007-11-19)
\itemize{
  \item Corrections in the documentation.

  \item Added description of the \code{graph6} format on \code{help(graph6)} page
}

\bold{Version 1.1} (2007-06-12)

\itemize{
  \item Corrected code for binary to decimal conversion. Previous version was
  returning wrong results for binary numbers that begin with 1 and have all
  other entries to 0.

  \item Added some tests for testing binary to decimal conversions
  as well as for converting matrices to graph6 format.
}


\bold{Version 1.0} (2007-06-06)

\itemize{
  \item Added functions \code{\link{b2d}} and \code{\link{d2b}} for
  conversions between decimal and binary numbers. They are based on
  compiled C code so should be much faster than the older ones
  written in \R.

  \item Functions \code{\link{bin2dec}} and \code{\link{dec2bin}}
  have been rewritten for use of newly added compiled code.

  \item Added a \code{sampleg6} file with couple of g6 symbols
}


\bold{Version 0.0-1} (2007-02-09)

\itemize{
  \item First beta version of the package
}

}

\keyword{package}
\keyword{math}
\examples{
# TODO add examples
}
