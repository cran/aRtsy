#' Paint a Turmite on a Canvas
#'
#' @description This function paints a turmite. A turmite is a Turing machine which has an orientation in addition to a current state and a "tape" that consists of a two-dimensional grid of cells. The algorithm is simple: 1) turn on the spot (left, right, up, down) 2) change the color of the square 3) move forward one square.
#'
#' @usage canvas_turmite(color, background = '#fafafa', p = 0.5, iterations = 1e7, 
#'                width = 1500, height = 1500)
#'
#' @param color   	  a character specifying the color used for the turmite.
#' @param background  a character specifying the color used for the background.
#' @param p           the probability of a state switch within the turmite.
#' @param iterations  the number of iterations of the turmite.
#' @param width       the width of the artwork in pixels.
#' @param height      the height of the artwork in pixels.
#'
#' @references \url{https://en.wikipedia.org/wiki/Turmite}
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#' canvas_turmite(color = "#000000", background = "#fafafa")
#' }
#' 
#' @keywords artwork canvas
#'
#' @export
#' @useDynLib aRtsy
#' @import Rcpp

canvas_turmite <- function(color, background = '#fafafa', p = 0.5, iterations = 1e7, 
                           width = 1500, height = 1500) {
  x <- y <- z <- NULL
  if (length(color) > 1)
    stop("This artwork can only take one color value.")
  if (length(background) > 1)
    stop("This artwork can only take one background value.")
  palette <- c(background, color)
  k <- sample(0:1, size = 1)
  row <- 0
  col <- 0
  if (k == 1)
    col <- sample(0:(width-1), size = 1)
  if (k == 0)
    row <- sample(0:(height-1), size = 1) 
  full_canvas <- iterate_turmite(matrix(0, nrow = height, ncol = width), iterations, row, col, p = p)  
  full_canvas <- reshape2::melt(full_canvas)
  colnames(full_canvas) <- c("y", "x", "z")
  artwork <- ggplot2::ggplot(data = full_canvas, ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_raster(interpolate = TRUE, alpha = 0.9) + 
    ggplot2::coord_equal() +
    ggplot2::scale_fill_gradientn(colours = palette) +
    ggplot2::scale_y_continuous(expand = c(0,0)) + 
    ggplot2::scale_x_continuous(expand = c(0,0))
  artwork <- themeCanvas(artwork, background = NULL)
  return(artwork)
}