% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/canvas_splits.R
\name{canvas_splits}
\alias{canvas_splits}
\title{Draw Split Lines}
\usage{
canvas_splits(
  colors,
  background = "#fafafa",
  iterations = 6,
  sd = 0.2,
  lwd = 0.05,
  alpha = 0.5
)
}
\arguments{
\item{colors}{a string or character vector specifying the color(s) used for the artwork.}

\item{background}{a character specifying the color used for the background (and the hole).}

\item{iterations}{a positive integer specifying the number of iterations of the algorithm.}

\item{sd}{a numeric value specifying the standard deviation of the angle noise.}

\item{lwd}{a numeric value specifying the width of the lines.}

\item{alpha}{a numeric value specifying the transparency of the lines.}
}
\value{
A \code{ggplot} object containing the artwork.
}
\description{
This function draws split lines.
}
\examples{
\donttest{
set.seed(2)

# Simple example
canvas_splits(colors = "black", sd = 0)

# Simple example
canvas_splits(colors = colorPalette("dark2"), background = "black", sd = 1)
}

}
\seealso{
\code{colorPalette}
}
\author{
Koen Derks, \email{koen-derks@hotmail.com}
}
\keyword{artwork}
\keyword{canvas}
