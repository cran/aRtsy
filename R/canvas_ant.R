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

#' Draw Langton's Ant
#'
#' @description This function draws Langton's Ant on a canvas. Langton's Ant is
#'   a two-dimensional cellular automaton that is named after its creator, Chris
#'   Langton. See the \code{Details} section for more specific information about
#'   the algorithm used in this function.
#'
#' @usage canvas_ant(
#'   colors,
#'   background = "#fafafa",
#'   iterations = 1000000,
#'   resolution = 500
#' )
#'
#' @param colors      a character (vector) specifying the color(s) used for the
#'   artwork.
#' @param background  a character specifying the color used for the background.
#' @param iterations  a positive integer specifying the number of iterations of
#'   the algorithm.
#' @param resolution  resolution of the artwork in pixels per row/column.
#'   Increasing the resolution increases the quality of the artwork but also
#'   increases the computation time exponentially.
#'
#' @return A \code{ggplot} object containing the artwork.
#'
#' @details The algorithm for Langton's Ant involves the following steps:
#'
#' \itemize{
#'   \item{Set up a two-dimensional grid of cells, where each cell can either be
#'     "colored" or "non-colored." The initial state of the grid is usually a
#'     single non-colored cell in the center of the grid.}
#'   \item{Place an "ant" on the grid at the position of the initial non-colored
#'     cell. The ant can move in four directions: up, down, left, or right.}
#'   \item{At each step of the algorithm, the ant examines the color of the cell
#'     it is currently on. If the cell is non-colored, the ant turns 90 degrees
#'     clockwise, colors the cell, and moves forward one unit.}
#'   \item{If the cell is colored, the ant turns 90 degrees counterclockwise,
#'     uncolors the cell, and moves forward one unit.}
#'   \item{The ant continues to move around the grid, following these rules at
#'     each step. If a certain number of iterations has passed, the ant chooses
#'     a different color which corresponds to a different combination of these
#'     rules.}
#' }
#'
#' @references \url{https://en.wikipedia.org/wiki/Langtons_ant}
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
#' canvas_ant(colors = colorPalette("house"))
#' }
#'
#' @export

canvas_ant <- function(colors,
                       background = "#fafafa",
                       iterations = 1000000,
                       resolution = 500) {
  .checkUserInput(
    background = background, resolution = resolution, iterations = iterations
  )
  if (iterations < length(colors)) {
    stop(paste0("'iterations' must be >= ", length(colors)))
  }
  palette <- c(background, colors)
  full_canvas <- cpp_ant(
    canvas = matrix(0, nrow = resolution, ncol = resolution),
    directions = .ant_directions(length(colors)),
    iterations = iterations,
    resolution = resolution
  )
  full_canvas <- .unraster(full_canvas, names = c("x", "y", "z"))
  artwork <- ggplot2::ggplot(data = full_canvas, ggplot2::aes(x = x, y = y, fill = z)) +
    ggplot2::geom_raster(interpolate = TRUE, alpha = 0.9) +
    ggplot2::xlim(c(0, resolution + 1)) +
    ggplot2::ylim(c(0, resolution + 1)) +
    ggplot2::scale_fill_gradientn(colours = palette)
  artwork <- theme_canvas(artwork, background)
  return(artwork)
}

.ant_directions <- function(n) {
  # Create a sequence of 0 (L) and 1 (R) positions
  sequence <- rep(0:1, n)
  # Create a matrix that holds all possible combinations of 0 (L) and 1 (R)
  pos <- expand.grid(sequence, sequence)
  # Remove all combinations that have two 0s or two 1s
  pos <- pos[which(pos[, 1] == pos[, 2]), ]
  # Mix the possible positions randomly
  pos[2:nrow(pos), ] <- pos[sample(2:nrow(pos), size = length(2:nrow(pos))), ]
  # Select only as many positions as there are colors given by the user
  pos <- pos[1:n, ]
  return(as.matrix(pos))
}
