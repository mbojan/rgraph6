\name{asAMatrix}
\alias{asAMatrix}
\alias{asAMatrix-methods}
\alias{asAMatrix,character-method}
\title{Convert a graph6 symbol to adjacency matrix}
\description{
This function takes a graph6 symbol and converts it to
a symmetric square adjacency matrix.
}
\usage{
asAMatrix(object)
\S4method{asAMatrix}{character}(object)
}
\arguments{
  \item{object}{character, a graph6 symbol}
}
\details{
For detailed explanations of graph6 format see \code{\link{graph6}}.
}
\value{
A square symmetric adjacency matrix representing the graph.
}
\references{\url{http://cs.anu.edu.au/people/bdm/data/formats.txt}}
\author{Michal Bojanowski \email{mbojan@ifispan.waw.pl}}
\note{This function aim at implementing format and methods originally created
by Brendan McKay, \email{bdm@cs.anu.edu.au} and
\url{http://cs.anu.edu.au/~bdm}.}
\seealso{\code{\link{graph6}} for format description and \code{\link{asGraph6}}
for conversion in the opposite direction.}
\examples{
data(sampleg6)
sampleg6
l <- lapply(sampleg6, asAMatrix)
l
}
\keyword{manip}
\keyword{methods}
