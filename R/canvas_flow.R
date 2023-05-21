# Copyright (C) 2021-2023 Koen Derks

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Draw A Flow Field
#'
#' @description This function draws flow fields on a canvas. The algorithm simulates the flow of points through a field of angles which can be set manually or generated from the predictions of a supervised learning method (i.e., knn, svm, random forest) trained on randomly generated data.
#'
#' @usage canvas_flow(
#'   colors,
#'   background = "#fafafa",
#'   lines = 500,
#'   lwd = 0.05,
#'   iterations = 100,
#'   stepmax = 0.01,
#'   outline = c("none", "circle", "square"),
#'   polar = FALSE,
#'   angles = NULL
#' )
#'
#' @param colors         a string or character vector specifying the color(s) used for the artwork.
#' @param background     a character specifying the color used for the background.
#' @param lines          the number of lines to draw.
#' @param lwd            expansion factor for the line width.
#' @param iterations     the maximum number of iterations for each line.
#' @param stepmax        the maximum proportion of the canvas covered in each iteration.
#' @param outline        character. Which outline to use for the artwork. Possible options are \code{none} (default), \code{circle} or \code{square}.
#' @param polar          logical. Whether to draw the flow field with polar coordinates.
#' @param angles         optional, a 200 x 200 matrix containing the angles in the flow field, or a character indicating the type of noise to use (\code{svm}, \code{knn}, \code{rf}, \code{perlin}, \code{cubic}, \code{simplex}, or \code{worley}). If \code{NULL} (the default), the noise type is chosen randomly.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @references \url{https://tylerxhobbs.com/essays/2020/flow-fields}
#'
#' @author Koen Derks, \email{koen-derks@hotmail.com}
#'
#' @keywords artwork canvas
#'
#' @seealso \code{colorPalette}
#'
#' @examples
#' \donttest{
#' set.seed(1)
#'
#' # Simple example
#' canvas_flow(colors = colorPalette("dark2"))
#'
#' # Outline example
#' canvas_flow(
#'   colors = colorPalette("vrolik1"), lines = 10000,
#'   outline = "circle", iterations = 10, angles = "svm"
#' )
#'
#' # Polar example
#' canvas_flow(
#'   colors = colorPalette("vrolik2"), lines = 300,
#'   lwd = 0.5, polar = TRUE
#' )
#'
#' # Advanced example
#' angles <- matrix(0, 200, 200)
#' angles[1:100, ] <- seq(from = 0, to = 2 * pi, length = 100)
#' angles[101:200, ] <- seq(from = 2 * pi, to = 0, length = 100)
#' angles <- angles + rnorm(200 * 200, sd = 0.1)
#' canvas_flow(
#'   colors = colorPalette("tuscany1"), background = "black",
#'   angles = angles, lwd = 0.4, lines = 1000, stepmax = 0.001
#' )
#' }
#'
#' @export

canvas_flow <- function(colors,
                        background = "#fafafa",
                        lines = 500,
                        lwd = 0.05,
                        iterations = 100,
                        stepmax = 0.01,
                        outline = c("none", "circle", "square"),
                        polar = FALSE,
                        angles = NULL) {
  .checkUserInput(
    background = background, iterations = iterations
  )
  outline <- match.arg(outline)
  sequence <- seq(0, 100, length = 100)
  grid <- expand.grid(sequence, sequence)
  grid <- data.frame(x = grid[, 1], y = grid[, 2], z = 0)
  left <- -100
  right <- 100
  bottom <- -100
  top <- 100
  ncols <- right - left
  nrows <- top - bottom
  if (is.null(angles)) {
    angles <- .noise(
      dims = c(nrows, ncols), n = sample(100:300, 1),
      type = sample(c("knn", "svm", "perlin", "cubic", "simplex", "worley"), 1),
      limits = c(-pi, pi)
    )
  } else if (is.character(angles)) {
    angles <- .noise(
      dims = c(nrows, ncols), n = sample(100:300, 1),
      type = angles,
      limits = c(-pi, pi)
    )
  } else {
    if (!is.matrix(angles)) {
      stop("'angles' must be a matrix")
    }
    if (nrow(angles) != nrows || ncol(angles) != ncols) {
      stop(paste0("'angles' must be a ", nrows, " x ", ncols, " matrix"))
    }
  }
  canvas <- matrix(NA, nrow = iterations * lines, ncol = 5)
  ncolors <- length(colors)
  canvas <- cpp_flow(canvas, angles, lines, iterations, ncolors, left, right, top, bottom, stepmax)
  canvas <- canvas[!is.na(canvas[, 1]), ]
  for (j in seq_len(lines)) {
    index <- which(canvas[, 3] == j)
    canvas[index, 5] <- .bmline(n = length(index), lwd)
  }
  canvas <- as.data.frame(canvas)
  colnames(canvas) <- c("x", "y", "z", "color", "size")
  canvas[["color"]] <- colors[canvas[["color"]]]
  if (outline == "circle") {
    canvas[which(sqrt(canvas[["x"]]^2 + canvas[["y"]]^2) > 175 / 2), "color"] <- background
  } else if (outline == "square") {
    canvas[which(canvas[["x"]] < -75 & canvas[["x"]] > 75 & canvas[["y"]] < -75 & canvas[["y"]] > 75), "color"] <- background
  }
  artwork <- ggplot2::ggplot(data = canvas, mapping = ggplot2::aes(x = x, y = y, group = factor(z))) +
    ggplot2::geom_path(linewidth = canvas[["size"]], color = canvas[["color"]], lineend = "round")
  if (polar) {
    artwork <- artwork + ggplot2::coord_polar()
  } else {
    artwork <- artwork + ggplot2::coord_cartesian(xlim = c(left, right), ylim = c(bottom, top))
  }
  artwork <- theme_canvas(artwork, background = background)
  return(artwork)
}
