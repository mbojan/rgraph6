\name{asGraph6}
\alias{asGraph6}
\alias{asGraph6-methods}
\alias{asGraph6,matrix-method}
\title{Convert adjacency matrix to a graph6 symbol}
\description{
This function converts a graph adjacency matrix to a graph6 symbol.
}
\usage{
asGraph6(object)
\S4method{asGraph6}{matrix}(object)
}
\arguments{
  \item{object}{a square adjacency matrix containing 0s and 1s}
}
\details{
 If necessary, more details than the description above
}
\value{
A graph6 symbol.
}
\references{\url{http://cs.anu.edu.au/people/bdm/data/formats.txt}}
\author{Michal Bojanowski \email{mbojan@ifispan.waw.pl}}
\note{This function aim at implementing format and methods originally created
by Brendan McKay, \email{bdm@cs.anu.edu.au} and
\url{http://cs.anu.edu.au/~bdm}.}
\seealso{\code{\link{graph6}} for format description and \code{\link{asAMatrix}}
for conversion in the opposite direction.}
\examples{
g <- asAMatrix("F?Dco")
g
asGraph6(g)
}
\keyword{manip}
\keyword{methods}
