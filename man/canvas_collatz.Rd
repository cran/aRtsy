% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canvas_collatz.R
\name{canvas_collatz}
\alias{canvas_collatz}
\title{Draw Collatz Sequences}
\usage{
canvas_collatz(
  colors,
  background = "#fafafa",
  n = 200,
  angle.even = 0.0075,
  angle.odd = 0.0145,
  side = FALSE
)
}
\arguments{
\item{colors}{a string or character vector specifying the color(s) used for the artwork.}

\item{background}{a character specifying the color used for the background.}

\item{n}{a positive integer specifying the number of random starting integers to use for the lines. Can also be a vector of numbers to use as starting numbers.}

\item{angle.even}{a value specifying the angle (in radials) to use in bending the sequence at each even number.}

\item{angle.odd}{a value specifying the angle (in radials) to use in bending the sequence at each odd number.}

\item{side}{logical. Whether to put the artwork on its side.}
}
\value{
A \code{ggplot} object containing the artwork.
}
\description{
This function draws the Collatz conjecture on a canvas. The
  conjecture of the Collatz sequence is that no matter what positive integer
  is chosen as the starting point of the sequence, the sequence will
  eventually reach the number 1. This conjecture has been verified for all
  starting integers up to very large numbers, but it has not been proven
  mathematically. Despite its simple rule, the sequence can produce long and
  complicated chains of numbers before eventually reaching 1. See the
  \code{Details} section for more specific information about the algorithm
  used in this function.
}
\details{
The Collatz sequence, also known as the \code{3n+1} problem, is a
  sequence of numbers generated by the following rule:

\itemize{
  \item{Start with any positive integer \code{n}.}
  \item{If \code{n} is even, divide it by 2.}
  \item{If \code{n} is odd, multiply it by 3 and add 1.}
  \item{Repeat this process with the new value of \code{n}, generating a new
    number in the sequence.}
}
}
\examples{
\donttest{
set.seed(1)

# Simple example
canvas_collatz(colors = colorPalette("tuscany3"))
}

}
\references{
\url{https://nl.wikipedia.org/wiki/Collatz_Conjecture}
}
\seealso{
\code{colorPalette}
}
\author{
Koen Derks, \email{koen-derks@hotmail.com}
}
\keyword{artwork}
\keyword{canvas}
