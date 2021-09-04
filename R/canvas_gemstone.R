#' Paint a Random Gemstone on a Canvas
#'
#' @description This function creates an artwork from randomly generated data by running a k-nearest neighbors regression algorithm to predict the color of each pixel on the canvas.
#'
#' @usage canvas_gemstone(colors, maxk = 1, n = 1000, resolution = 500)
#'
#' @param colors      a string or character vector specifying the color(s) used for the artwork.
#' @param maxk        a positive integer specifying the maximum number of nearest neighbors to consider.
#' @param n           a positive integer specifying the number of random data points to generate.
#' @param resolution  a positive integer specifying the number of pixels (resolution x resolution) of the artwork.
#'
#' @references \url{https://en.wikipedia.org/wiki/K-nearest_neighbors_algorithm}
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @examples
#' \donttest{
#' set.seed(7)
#' palette <- colorPalette('random', n = 10)
#' canvas_gemstone(colors = palette)
#' }
#' 
#' @keywords artwork canvas
#'
#' @export
#' @importFrom stats predict

canvas_gemstone <- function(colors, maxk = 1, n = 1000, resolution = 500) {
  x <- y <- z <- NULL # Global variables
  train <- data.frame(x = stats::runif(n, 0, 1), # Create a training data set with x (predictor), y (predictor), z (response)
                      y = stats::runif(n, 0, 1), 
                      z = stats::runif(n, 0, 1))
  fit <- kknn::train.kknn(formula = z ~ x + y, data = train) # Fit knn model to training data
  sequence <- seq(0, 1, length = resolution) # Create a sequence of pixels
  canvas <- expand.grid(sequence, sequence) # Create all combinations of pixels
  colnames(canvas) <- c("x", "y")
  z <- predict(fit, newdata = canvas) # Predict each pixel using the fitted model
  full_canvas <- data.frame(x = canvas$x, y = canvas$y, z = z)
  artwork <- ggplot2::ggplot(data = full_canvas, mapping = ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_tile() +
    ggplot2::xlim(c(0, 1)) +
    ggplot2::ylim(c(0, 1)) +
    ggplot2::scale_fill_gradientn(colours = colors)
  artwork <- theme_canvas(artwork, background = NULL)
  return(artwork)
}