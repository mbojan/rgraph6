\name{b2d}
\alias{b2d}
\alias{d2b}
\title{Conversions between binary and decimal numbers}
\description{
These functions convert between binary numbers (as numeric vectors) and decimal
numbers.
}
\usage{
b2d(bin)
d2b(dec)
}
\arguments{
  \item{bin}{numeric vector of 0s and 1s representing the number
    in binary format}
  \item{dec}{numeric non-negative scalar, decimal number to be converted}
}
\value{
For \code{b2d} the decimal number representing the binary input.  For
\code{d2b} the numeric vector representing the decimal as binary number:
sequence of 0s and 1s.
}
\seealso{\code{\link{bin2dec}} and \code{\link{dec2bin}} for more
user friendly interface.}
\examples{
b2d(c(1, 1, 0, 1))
d2b(13)
}
\keyword{math}

