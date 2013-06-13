\name{bin2dec}
\alias{bin2dec}
\alias{dec2bin}
\alias{bin2dec-methods}
\alias{bin2dec,numeric-method}
\alias{bin2dec,character-method}
\title{Converting between decimal and binary numbers}
\description{
These functions provide user-friendly interface to low-level
routines \code{b2d} and \code{d2b} for converting
between decimal and binary numbers.
}
\usage{
dec2bin(dec, num = TRUE)
bin2dec(bin)
\S4method{bin2dec}{numeric}(bin)
\S4method{bin2dec}{character}(bin)
}
\arguments{
  \item{bin}{binary number as a numeric vector of 0s and 1s or a single
  character string containing 0s and 1s only}
  \item{dec}{single decimal number to be converted}
  \item{num}{logical, whether a numeric vector (instead of single character string)
  should be returned}
}
\value{
For \code{bin2dec} the decimal number representing the binary input.

For \code{dec2bin}, if \code{num} is \code{TRUE}, which is the default,
the numeric vector containing the binary number. If \code{num} is
\code{FALSE} then a single string containing the sequence of 0s and
1s is returned.
}
\seealso{\code{\link{b2d}} and \code{\link{d2b}} for low-level procedures}
\examples{
bin2dec("1101")
bin2dec(c(1,1,0,1))

dec2bin(13)
dec2bin(13, FALSE)
}
\keyword{math}
