\name{sampleg6}
\alias{sampleg6}
\docType{data}
\title{A sample vector of g6 codes}
\description{
A vector of graph6 symbols each representing an undirected graph.
}
\usage{data(sampleg6)}
\format{
  The format is:
 chr [1:9] "CR" "CJ" "CN" ...
}
\seealso{
See \code{\link{graph6}} for format description, \code{\link{asAMatrix}} and
\code{\link{asGraph6}} for conversion routines.
}
\examples{
data(sampleg6)
l <- lapply( sampleg6, asAMatrix )
# list of adjacency matrices
str(l)
}
\keyword{datasets}
